/*
 * Copyright (c) 2004, 2018, Oracle and/or its affiliates. All rights reserved.
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

#ifdef __cplusplus
extern "C" {
#endif

/* ========================================================================== */

/* scaffold objects */
static jlong timeout = 0;

/* test objects */
static int eventsCount = 0;

/* ========================================================================== */

/* check if any VMObjectAlloc events received */
static int checkVMObjectAllocEvents() {

    NSK_DISPLAY1("VMObjectAlloc events received: %d\n", eventsCount);

    if (eventsCount == 0) {
        NSK_DISPLAY0("# WARNING: no VMObjectAlloc events\n");
        NSK_DISPLAY0("#    (VM might not allocate such objects at all)\n");
    }

    return NSK_TRUE;
}

/* ========================================================================== */

JNIEXPORT void JNICALL
VMObjectAlloc(jvmtiEnv *jvmti, JNIEnv* jni, jthread thread, jobject object,
              jclass object_klass, jlong size) {
    char *signature, *generic;

    eventsCount++;

    if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB4(GetClassSignature, jvmti,
            object_klass, &signature, &generic))) {
        nsk_jvmti_setFailStatus();
        return;
    }

    NSK_DISPLAY2("VMObjectAlloc: \"%s\", size=%d\n", signature, size);

    if (signature != NULL)
        NSK_CPP_STUB2(Deallocate, jvmti, (unsigned char*)signature);

    if (generic != NULL)
        NSK_CPP_STUB2(Deallocate, jvmti, (unsigned char*)generic);

}

/* ========================================================================== */

/* agent algorithm */
static void JNICALL
agentProc(jvmtiEnv* jvmti, JNIEnv* jni, void* arg) {

    /* wait for debuggee start */
    if (!nsk_jvmti_waitForSync(timeout))
        return;

    /* testcase #1: check if any VMObjectAlloc events received*/
    NSK_DISPLAY0("Testcase #1: check if any VMObjectAlloc events received\n");
    if (!checkVMObjectAllocEvents())
        nsk_jvmti_setFailStatus();

    /* resume debugee after last sync */
    if (!nsk_jvmti_resumeSync())
        return;
}

/* ========================================================================== */

/* agent library initialization */
#ifdef STATIC_BUILD
JNIEXPORT jint JNICALL Agent_OnLoad_vmobjalloc001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNICALL Agent_OnAttach_vmobjalloc001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNI_OnLoad_vmobjalloc001(JavaVM *jvm, char *options, void *reserved) {
    return JNI_VERSION_1_8;
}
#endif
jint Agent_Initialize(JavaVM *jvm, char *options, void *reserved) {
    jvmtiEnv* jvmti = NULL;
    jvmtiCapabilities caps;
    jvmtiEventCallbacks callbacks;

    /* init framework and parse options */
    if (!NSK_VERIFY(nsk_jvmti_parseOptions(options)))
        return JNI_ERR;

    timeout = nsk_jvmti_getWaitTime() * 60000;
    NSK_DISPLAY1("Timeout: %d msc\n", (int)timeout);

    /* create JVMTI environment */
    if (!NSK_VERIFY((jvmti =
            nsk_jvmti_createJVMTIEnv(jvm, reserved)) != NULL))
        return JNI_ERR;

    memset(&caps, 0, sizeof(caps));
    caps.can_generate_vm_object_alloc_events = 1;
    if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB2(AddCapabilities, jvmti, &caps))) {
        return JNI_ERR;
    }

    memset(&callbacks, 0, sizeof(callbacks));
    callbacks.VMObjectAlloc= &VMObjectAlloc;
    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB3(SetEventCallbacks, jvmti,
                &callbacks, sizeof(callbacks))))
        return JNI_ERR;

    /* enable VMObjectAlloc event */
    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB4(SetEventNotificationMode, jvmti, JVMTI_ENABLE,
                JVMTI_EVENT_VM_OBJECT_ALLOC, NULL)))
        return JNI_ERR;

    /* register agent proc and arg */
    if (!NSK_VERIFY(nsk_jvmti_setAgentProc(agentProc, NULL)))
        return JNI_ERR;

    return JNI_OK;
}

/* ========================================================================== */

#ifdef __cplusplus
}
#endif
