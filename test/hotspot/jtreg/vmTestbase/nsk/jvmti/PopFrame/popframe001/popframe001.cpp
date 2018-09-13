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

#include <stdio.h>
#include <string.h>
#include "jvmti.h"
#include "agent_common.h"
#include "JVMTITools.h"

extern "C" {


#define STATUS_FAILED 2
#define PASSED 0

static jvmtiEnv *jvmti = NULL;
static jvmtiCapabilities caps;
static jvmtiEventCallbacks callbacks;
static int watch_ev = 0;        /* ignore JVMTI events by default */
static int gen_ev = 0;          /* number of generated events */
static int tot_result = PASSED; /* total result of the test */

static jrawMonitorID watch_ev_monitor;

static void set_watch_ev(int value) {
    jvmti->RawMonitorEnter(watch_ev_monitor);

    watch_ev = value;

    jvmti->RawMonitorExit(watch_ev_monitor);
}

void JNICALL
FramePop(jvmtiEnv *jvmti_env, JNIEnv *env, jthread thread,
        jmethodID method, jboolean wasPopedByException) {
    jvmti->RawMonitorEnter(watch_ev_monitor);

    if (watch_ev) {
        printf("#### FramePop event occurred ####\n");
        gen_ev++;
    }

    jvmti->RawMonitorExit(watch_ev_monitor);
}

void JNICALL
MethodExit(jvmtiEnv *jvmti_env, JNIEnv *env, jthread thr,
        jmethodID method, jboolean was_poped_by_exc, jvalue return_value) {
    jvmti->RawMonitorEnter(watch_ev_monitor);

    if (watch_ev) {
        printf("#### MethodExit event occurred ####\n");
        gen_ev++;
    }

    jvmti->RawMonitorExit(watch_ev_monitor);
}

JNIEXPORT jint JNICALL
Java_nsk_jvmti_PopFrame_popframe001_suspThread(JNIEnv *env,
        jclass cls, jint vrb, jobject susThr) {
    jvmtiError err;

    if (!caps.can_pop_frame || !caps.can_suspend) {
        return PASSED;
    }

    if (vrb == 1)
        printf(">>>>>>>> Invoke SuspendThread()\n");
    if ((err = (jvmti->SuspendThread(susThr))) != JVMTI_ERROR_NONE) {
        printf("%s: Failed to call SuspendThread(): error=%d: %s\n",
            __FILE__, err, TranslateError(err));
        return JNI_ERR;
    }
    if (vrb == 1)
        printf("<<<<<<<< SuspendThread() is successfully done\n");
    return PASSED;
}

JNIEXPORT jint JNICALL
Java_nsk_jvmti_PopFrame_popframe001_resThread(JNIEnv *env,
        jclass cls, jint vrb, jobject susThr) {
    jvmtiError err;

    if (!caps.can_pop_frame || !caps.can_suspend) {
        return PASSED;
    }

    if (vrb == 1)
        printf(">>>>>>>> Invoke ResumeThread()\n");
    if ((err = (jvmti->ResumeThread(susThr))) != JVMTI_ERROR_NONE) {
        printf("%s: Failed to call ResumeThread(): error=%d: %s\n",
            __FILE__, err, TranslateError(err));
        return JNI_ERR;
    }
    if (vrb == 1)
        printf("<<<<<<<< ResumeThread() is successfully done\n");
    return PASSED;
}

JNIEXPORT jint JNICALL
Java_nsk_jvmti_PopFrame_popframe001_doPopFrame(JNIEnv *env,
        jclass cls, jint vrb, jobject frameThr) {
    jvmtiError err;

    if (!caps.can_pop_frame || !caps.can_suspend) {
        return PASSED;
    }

    err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
        JVMTI_EVENT_METHOD_EXIT, frameThr);
    if (err != JVMTI_ERROR_NONE) {
        printf("Failed to enable METHOD_EXIT event: %s (%d)\n",
               TranslateError(err), err);
        tot_result = STATUS_FAILED;
    }

    err = jvmti->SetEventNotificationMode(JVMTI_ENABLE,
        JVMTI_EVENT_FRAME_POP, frameThr);
    if (err != JVMTI_ERROR_NONE) {
        printf("Failed to enable FRAME_POP event: %s (%d)\n",
               TranslateError(err), err);
        tot_result = STATUS_FAILED;
    }

    if (vrb == 1)
        printf(">>>>>>>> Invoke PopFrame()\n");
    set_watch_ev(1); /* watch JVMTI events */

    if ((err = (jvmti->PopFrame(frameThr))) != JVMTI_ERROR_NONE) {
        printf("TEST FAILED: the function PopFrame() returned the error %d: %s\n",
            err, TranslateError(err));
        printf("\tFor more info about this error see the JVMTI spec.\n");
        tot_result = STATUS_FAILED;
    }
    else if (vrb == 1)
        printf("Check #1 PASSED: PopFrame() is successfully done\n");

    set_watch_ev(0); /* ignore again JVMTI events */
    if (gen_ev) {
        printf("TEST FAILED: %d JVMTI events were generated by the function PopFrame()\n",
            gen_ev);
        tot_result = STATUS_FAILED;
    } else if (vrb == 1)
        printf("Check #2 PASSED: No JVMTI events were generated by the function PopFrame()\n");

    return(tot_result);
}

#ifdef STATIC_BUILD
JNIEXPORT jint JNICALL Agent_OnLoad_popframe001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNICALL Agent_OnAttach_popframe001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNI_OnLoad_popframe001(JavaVM *jvm, char *options, void *reserved) {
    return JNI_VERSION_1_8;
}
#endif
jint  Agent_Initialize(JavaVM *jvm, char *options, void *reserved) {
    jint res;
    jvmtiError err;

    res = jvm->GetEnv((void **) &jvmti, JVMTI_VERSION_1_1);
    if (res != JNI_OK || jvmti == NULL) {
        printf("Wrong result of a valid call to GetEnv!\n");
        return JNI_ERR;
    }

    err = jvmti->GetPotentialCapabilities(&caps);
    if (err != JVMTI_ERROR_NONE) {
        printf("(GetPotentialCapabilities) unexpected error: %s (%d)\n",
               TranslateError(err), err);
        return JNI_ERR;
    }

    err = jvmti->AddCapabilities(&caps);
    if (err != JVMTI_ERROR_NONE) {
        printf("(AddCapabilities) unexpected error: %s (%d)\n",
               TranslateError(err), err);
        return JNI_ERR;
    }

    err = jvmti->GetCapabilities(&caps);
    if (err != JVMTI_ERROR_NONE) {
        printf("(GetCapabilities) unexpected error: %s (%d)\n",
               TranslateError(err), err);
        return JNI_ERR;
    }

    if (!caps.can_pop_frame) {
        printf("Warning: PopFrame is not implemented\n");
        return JNI_OK;
    }

    if (!caps.can_suspend) {
        printf("Warning: suspend/resume is not implemented\n");
        return JNI_OK;
    }

    if (caps.can_generate_frame_pop_events &&
            caps.can_generate_method_exit_events) {
        callbacks.MethodExit = &MethodExit;
        callbacks.FramePop = &FramePop;
        err = jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));
        if (err != JVMTI_ERROR_NONE) {
            printf("(SetEventCallbacks) unexpected error: %s (%d)\n",
                   TranslateError(err), err);
            return JNI_ERR;
        }
    } else {
        printf("Warning: FramePop or MethodExit event is not implemented\n");
    }

    err = jvmti->CreateRawMonitor("watch_ev_monitor", &watch_ev_monitor);
    if (err != JVMTI_ERROR_NONE) {
        printf("(CreateRawMonitor) unexpected error: %s (%d)\n",
               TranslateError(err), err);
        return JNI_ERR;
    }

    return JNI_OK;
}

}
