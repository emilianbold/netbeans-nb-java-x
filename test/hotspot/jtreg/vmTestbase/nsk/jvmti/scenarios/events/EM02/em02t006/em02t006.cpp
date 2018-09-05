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
#include "JVMTITools.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================= */

/* scaffold objects */
static jvmtiEnv *jvmti = NULL;
static jlong timeout = 0;
static jrawMonitorID syncLock = NULL;

/* constant names */
#define STEP_NUMBER 3
#define OBJECT_NUMBER 100
#define JVMTI_EVENT_COUNT   (int)(JVMTI_MAX_EVENT_TYPE_VAL - JVMTI_MIN_EVENT_TYPE_VAL + 1)

static int eventCount[JVMTI_EVENT_COUNT];
static int newEventCount[JVMTI_EVENT_COUNT];

/* ============================================================================= */


/*
 * Class:     nsk_jvmti_scenarios_events_EM02_em02t006
 * Method:    setTag
 * Signature: (Ljava/lang/Object;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_nsk_jvmti_scenarios_events_EM02_em02t006_setTag(JNIEnv *env,
                        jobject o, jobject object, jlong tag) {

    if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB3(SetTag, jvmti, object, tag))) {
        NSK_COMPLAIN0("TEST FAILED: unable to set tag for a tested object\n");
        return NSK_FALSE;
    }
    return NSK_TRUE;
}

static void
showEventStatistics(int step) {
    int i;
    const char* str;
    int *currentCounts = (step == 1) ? &eventCount[0] : &newEventCount[0];

    NSK_DISPLAY0("\n");
    NSK_DISPLAY1("Event statistics for %d step:\n", step);
    NSK_DISPLAY0("-----------------------------\n");
    for (i = 0; i < JVMTI_EVENT_COUNT; i++) {
        if (currentCounts[i] > 0) {
            str = TranslateEvent((jvmtiEvent)(i+JVMTI_MIN_EVENT_TYPE_VAL));
            NSK_DISPLAY2("%-40s %7d\n", str, currentCounts[i]);
        }
    }
}

/* ========================================================================== */

int checkEvents(int step) {
    int i;
    jvmtiEvent curr;
    int result = NSK_TRUE;
    int *currentCounts;
    int isExpected = 0;

    switch (step) {
        case 1:
            currentCounts = &eventCount[0];
            break;

        case 2:
        case 3:
            currentCounts = &newEventCount[0];
            break;

        default:
            NSK_COMPLAIN1("Unexpected step no: %d\n", step);
            return NSK_FALSE;
    }

    for (i = 0; i < JVMTI_EVENT_COUNT; i++) {

        curr = (jvmtiEvent) (i + JVMTI_MIN_EVENT_TYPE_VAL);

        switch (step) {
            case 1:
                isExpected = ((curr == JVMTI_EVENT_OBJECT_FREE)
                                || (curr == JVMTI_EVENT_VM_INIT));
                break;

            case 2:
                isExpected = (curr == JVMTI_EVENT_OBJECT_FREE);
                break;

            case 3:
                isExpected = (curr == JVMTI_EVENT_VM_DEATH);
                break;
        }

        if (isExpected) {
            if (currentCounts[i] < 0 || currentCounts[i] > OBJECT_NUMBER) {
                    NSK_COMPLAIN2("Unexpected events number %7d for %s\n\texpected value must be greater than 1\n",
                                        currentCounts[i],
                                        TranslateEvent(curr));
                result = NSK_FALSE;
            }
        } else {

            if (currentCounts[i] > 0) {
                NSK_COMPLAIN2("Unexpected event %s was sent %d times\n",
                                    TranslateEvent(curr),
                                    currentCounts[i]);
                result = NSK_FALSE;
            }
        }
    }

    return result;
}

static void
changeCount(jvmtiEvent event, int *currentCounts) {

    if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB2(RawMonitorEnter, jvmti, syncLock)))
        nsk_jvmti_setFailStatus();

    currentCounts[event - JVMTI_MIN_EVENT_TYPE_VAL]++;

    if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB2(RawMonitorExit, jvmti, syncLock)))
        nsk_jvmti_setFailStatus();

}

/* ============================================================================= */

/* callbacks */
JNIEXPORT void JNICALL
cbVMInit(jvmtiEnv* jvmti, JNIEnv* jni_env, jthread thread) {
    changeCount(JVMTI_EVENT_VM_INIT, &eventCount[0]);
}

JNIEXPORT void JNICALL
cbVMDeath(jvmtiEnv* jvmti, JNIEnv* jni_env) {
    changeCount(JVMTI_EVENT_VM_DEATH, &newEventCount[0]);
    showEventStatistics(STEP_NUMBER);
    if (!checkEvents(STEP_NUMBER))
        nsk_jvmti_setFailStatus();

    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB2(DestroyRawMonitor, jvmti, syncLock)))
        nsk_jvmti_setFailStatus();

}

void JNICALL
cbException(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location, jobject exception,
                jmethodID catch_method, jlocation catch_location) {
    changeCount(JVMTI_EVENT_EXCEPTION, &eventCount[0]);
}

void JNICALL
cbExceptionCatch(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location, jobject exception) {
    changeCount(JVMTI_EVENT_EXCEPTION_CATCH, &eventCount[0]);
}

void JNICALL
cbSingleStep(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location) {
    changeCount(JVMTI_EVENT_SINGLE_STEP, &eventCount[0]);
}

void JNICALL
cbFramePop(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jboolean was_popped_by_exception) {
    changeCount(JVMTI_EVENT_FRAME_POP, &eventCount[0]);
}

void JNICALL
cbBreakpoint(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location) {
    changeCount(JVMTI_EVENT_BREAKPOINT, &eventCount[0]);
}

void JNICALL
cbFieldAccess(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location, jclass field_klass,
                jobject object, jfieldID field) {
    changeCount(JVMTI_EVENT_FIELD_ACCESS, &eventCount[0]);
}

void JNICALL
cbFieldModification(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jlocation location, jclass field_klass,
                jobject object, jfieldID field, char signature_type,
                jvalue new_value) {
    changeCount(JVMTI_EVENT_FIELD_MODIFICATION, &eventCount[0]);
}

void JNICALL
cbMethodEntry(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method) {
    changeCount(JVMTI_EVENT_METHOD_ENTRY, &eventCount[0]);
}

void JNICALL
cbMethodExit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                jmethodID method, jboolean was_popped_by_exception,
                jvalue return_value) {
    changeCount(JVMTI_EVENT_METHOD_EXIT, &eventCount[0]);
}

void JNICALL
cbNativeMethodBind(jvmtiEnv *jvmti_env, JNIEnv* jni_env,jthread thread,
                jmethodID method, void* address, void** new_address_ptr) {
    changeCount(JVMTI_EVENT_NATIVE_METHOD_BIND, &eventCount[0]);
}

void JNICALL
cbMonitorWait(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                    jobject object, jlong tout) {

    changeCount(JVMTI_EVENT_MONITOR_WAIT, &eventCount[0]);
}

void JNICALL
cbMonitorWaited(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                    jobject object, jboolean timed_out) {

    changeCount(JVMTI_EVENT_MONITOR_WAITED, &eventCount[0]);
}

JNIEXPORT void JNICALL
cbMonitorContendedEnter(jvmtiEnv* jvmti, JNIEnv* jni_env, jthread thread,
                            jobject object) {

    changeCount(JVMTI_EVENT_MONITOR_CONTENDED_ENTER, &eventCount[0]);
}

void JNICALL
cbMonitorContendedEntered(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                            jobject object) {

    changeCount(JVMTI_EVENT_MONITOR_CONTENDED_ENTERED, &eventCount[0]);
}

void JNICALL
cbCompiledMethodLoad(jvmtiEnv *jvmti_env, jmethodID method, jint code_size,
                const void* code_addr, jint map_length,
                const jvmtiAddrLocationMap* map, const void* compile_info) {
    changeCount(JVMTI_EVENT_COMPILED_METHOD_LOAD, &eventCount[0]);
}

void JNICALL
cbCompiledMethodUnload(jvmtiEnv *jvmti_env, jmethodID method,
                const void* code_addr) {
    changeCount(JVMTI_EVENT_COMPILED_METHOD_UNLOAD, &eventCount[0]);
}

void JNICALL
cbGarbageCollectionStart(jvmtiEnv *jvmti_env) {
    changeCount(JVMTI_EVENT_GARBAGE_COLLECTION_START, &eventCount[0]);
}

void JNICALL
cbGarbageCollectionFinish(jvmtiEnv *jvmti_env) {
    changeCount(JVMTI_EVENT_GARBAGE_COLLECTION_FINISH, &eventCount[0]);
}

void JNICALL
cbObjectFree(jvmtiEnv *jvmti_env, jlong tag) {

    changeCount(JVMTI_EVENT_OBJECT_FREE, &eventCount[0]);
    if (tag < 1 || tag > OBJECT_NUMBER) {
        NSK_COMPLAIN1("Unexpected tag value %lld\n", tag);
        nsk_jvmti_setFailStatus();
    }
}

void JNICALL
cbNewObjectFree(jvmtiEnv *jvmti_env, jlong tag) {

    changeCount(JVMTI_EVENT_OBJECT_FREE, &newEventCount[0]);
    if (tag < 1 || tag > OBJECT_NUMBER) {
        NSK_COMPLAIN1("Unexpected tag value %lld\n", tag);
        nsk_jvmti_setFailStatus();
    }
}

void JNICALL
cbVMObjectAlloc(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread,
                    jobject object, jclass object_klass, jlong size) {

    changeCount(JVMTI_EVENT_VM_OBJECT_ALLOC, &eventCount[0]);
}

/* ============================================================================= */

static int enableEvent(jvmtiEvent event) {

    if (nsk_jvmti_isOptionalEvent(event)
            && (event != JVMTI_EVENT_OBJECT_FREE)) {
        if (!NSK_JVMTI_VERIFY_CODE(JVMTI_ERROR_MUST_POSSESS_CAPABILITY,
                NSK_CPP_STUB4(SetEventNotificationMode, jvmti,
                    JVMTI_ENABLE, event, NULL))) {
            NSK_COMPLAIN1("Unexpected error enabling %s\n",
                TranslateEvent(event));
            return NSK_FALSE;
        }
    } else {
        if (!NSK_JVMTI_VERIFY(
                NSK_CPP_STUB4(SetEventNotificationMode, jvmti,
                    JVMTI_ENABLE, event, NULL))) {
            NSK_COMPLAIN1("Unexpected error enabling %s\n",
                TranslateEvent(event));
            return NSK_FALSE;
        }
    }

    return NSK_TRUE;
}

/**
 * Enable or disable tested events.
 */
static int enableEventList() {

    int i, result;

    result = enableEvent(JVMTI_EVENT_VM_INIT);

    result = result && enableEvent(JVMTI_EVENT_VM_DEATH);

    /* enabling optional events */
    for (i = 0; i < JVMTI_EVENT_COUNT; i++) {
        jvmtiEvent event = (jvmtiEvent)(i+JVMTI_MIN_EVENT_TYPE_VAL);

        if (nsk_jvmti_isOptionalEvent(event))
            result = result && enableEvent(event);
    }

    if (result == NSK_FALSE) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }

    return NSK_TRUE;
}

/* ============================================================================= */

static int
setCallBacks(int step) {

    int i;

    jvmtiEventCallbacks eventCallbacks;
    memset(&eventCallbacks, 0, sizeof(eventCallbacks));

    switch (step) {
        case 1:
            for (i = 0; i < JVMTI_EVENT_COUNT; i++) {
                eventCount[i] = 0;
            }

            eventCallbacks.VMInit                    = cbVMInit;
            eventCallbacks.Exception                 = cbException;
            eventCallbacks.ExceptionCatch            = cbExceptionCatch;
            eventCallbacks.SingleStep                = cbSingleStep;
            eventCallbacks.FramePop                  = cbFramePop;
            eventCallbacks.Breakpoint                = cbBreakpoint;
            eventCallbacks.FieldAccess               = cbFieldAccess;
            eventCallbacks.FieldModification         = cbFieldModification;
            eventCallbacks.MethodEntry               = cbMethodEntry;
            eventCallbacks.MethodExit                = cbMethodExit;
            eventCallbacks.NativeMethodBind          = cbNativeMethodBind;
            eventCallbacks.CompiledMethodLoad        = cbCompiledMethodLoad;
            eventCallbacks.CompiledMethodUnload      = cbCompiledMethodUnload;
            eventCallbacks.MonitorWait               = cbMonitorWait;
            eventCallbacks.MonitorWaited             = cbMonitorWaited;
            eventCallbacks.MonitorContendedEnter     = cbMonitorContendedEnter;
            eventCallbacks.MonitorContendedEntered   = cbMonitorContendedEntered;
            eventCallbacks.GarbageCollectionStart    = cbGarbageCollectionStart;
            eventCallbacks.GarbageCollectionFinish   = cbGarbageCollectionFinish;
            eventCallbacks.ObjectFree                = cbObjectFree;
            eventCallbacks.VMObjectAlloc             = cbVMObjectAlloc;
            break;

        case 2:
            for (i = 0; i < JVMTI_EVENT_COUNT; i++) {
                newEventCount[i] = 0;
            }

            eventCallbacks.ObjectFree                = cbNewObjectFree;
            break;

        case 3:
            for (i = 0; i < JVMTI_EVENT_COUNT; i++) {
                newEventCount[i] = 0;
            }

            eventCallbacks.VMDeath                   = cbVMDeath;
            break;

    }
    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB3(SetEventCallbacks, jvmti,
                                &eventCallbacks,
                                sizeof(eventCallbacks))))
        return NSK_FALSE;

    return NSK_TRUE;
}

/* ============================================================================= */

/** Agent algorithm. */
static void JNICALL
agentProc(jvmtiEnv* jvmti, JNIEnv* agentJNI, void* arg) {

    int i;

    for (i = 1; i <= STEP_NUMBER; i++) {

        if (!nsk_jvmti_waitForSync(timeout))
            return;

        NSK_DISPLAY0("Check received events\n");

        if (i < STEP_NUMBER) {
            showEventStatistics(i);
            if (!checkEvents(i))
                nsk_jvmti_setFailStatus();

            if (!setCallBacks(i + 1)) {
                return;
            }
        }

        if (!nsk_jvmti_resumeSync())
            return;
    }

}

/* ============================================================================= */

/** Agent library initialization. */
#ifdef STATIC_BUILD
JNIEXPORT jint JNICALL Agent_OnLoad_em02t006(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNICALL Agent_OnAttach_em02t006(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNI_OnLoad_em02t006(JavaVM *jvm, char *options, void *reserved) {
    return JNI_VERSION_1_8;
}
#endif
jint Agent_Initialize(JavaVM *jvm, char *options, void *reserved) {

    if (!NSK_VERIFY(nsk_jvmti_parseOptions(options)))
        return JNI_ERR;

    timeout = nsk_jvmti_getWaitTime() * 60 * 1000;

    if (!NSK_VERIFY((jvmti = nsk_jvmti_createJVMTIEnv(jvm, reserved)) != NULL))
        return JNI_ERR;

    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB3(CreateRawMonitor, jvmti, "_syncLock", &syncLock))) {
        nsk_jvmti_setFailStatus();
        return JNI_ERR;
    }

    {
        jvmtiCapabilities caps;
        memset(&caps, 0, sizeof(caps));

        caps.can_tag_objects = 1;
        caps.can_generate_object_free_events = 1;
        if (!NSK_JVMTI_VERIFY(NSK_CPP_STUB2(AddCapabilities, jvmti, &caps)))
            return JNI_ERR;
    }

    if (!setCallBacks(1)) {
        return JNI_ERR;
    }

    if (!enableEventList()) {
        return JNI_ERR;
    }

    if (!NSK_VERIFY(nsk_jvmti_setAgentProc(agentProc, NULL)))
        return JNI_ERR;

    return JNI_OK;
}

/* ============================================================================= */


#ifdef __cplusplus
}
#endif
