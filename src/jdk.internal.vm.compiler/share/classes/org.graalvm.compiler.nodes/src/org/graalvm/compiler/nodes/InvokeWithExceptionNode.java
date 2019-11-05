/*
 * Copyright (c) 2011, 2019, Oracle and/or its affiliates. All rights reserved.
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


package org.graalvm.compiler.nodes;

import static org.graalvm.compiler.nodeinfo.InputType.Extension;
import static org.graalvm.compiler.nodeinfo.InputType.Memory;
import static org.graalvm.compiler.nodeinfo.InputType.State;
import static org.graalvm.compiler.nodeinfo.NodeCycles.CYCLES_UNKNOWN;
import static org.graalvm.compiler.nodeinfo.NodeSize.SIZE_UNKNOWN;

import java.util.Map;

import org.graalvm.compiler.core.common.type.Stamp;
import org.graalvm.compiler.debug.DebugCloseable;
import org.graalvm.compiler.graph.IterableNodeType;
import org.graalvm.compiler.graph.Node;
import org.graalvm.compiler.graph.NodeClass;
import org.graalvm.compiler.nodeinfo.NodeInfo;
import org.graalvm.compiler.nodeinfo.Verbosity;
import org.graalvm.compiler.nodes.java.MethodCallTargetNode;
import org.graalvm.compiler.nodes.memory.MemoryCheckpoint;
import org.graalvm.compiler.nodes.spi.LIRLowerable;
import org.graalvm.compiler.nodes.spi.LoweringTool;
import org.graalvm.compiler.nodes.spi.NodeLIRBuilderTool;
import org.graalvm.compiler.nodes.spi.UncheckedInterfaceProvider;
import org.graalvm.compiler.nodes.util.GraphUtil;
import jdk.internal.vm.compiler.word.LocationIdentity;

import jdk.vm.ci.code.BytecodeFrame;

@NodeInfo(nameTemplate = "Invoke!#{p#targetMethod/s}", allowedUsageTypes = {Memory}, cycles = CYCLES_UNKNOWN, size = SIZE_UNKNOWN)
public final class InvokeWithExceptionNode extends ControlSplitNode implements Invoke, IterableNodeType, MemoryCheckpoint.Single, LIRLowerable, UncheckedInterfaceProvider {
    public static final NodeClass<InvokeWithExceptionNode> TYPE = NodeClass.create(InvokeWithExceptionNode.class);

    private static final double EXCEPTION_PROBA = 1e-5;

    @Successor AbstractBeginNode next;
    @Successor AbstractBeginNode exceptionEdge;
    @OptionalInput ValueNode classInit;
    @Input(Extension) CallTargetNode callTarget;
    @OptionalInput(State) FrameState stateDuring;
    @OptionalInput(State) FrameState stateAfter;
    protected int bci;
    protected boolean polymorphic;
    protected boolean useForInlining;
    protected double exceptionProbability;

    public InvokeWithExceptionNode(CallTargetNode callTarget, AbstractBeginNode exceptionEdge, int bci) {
        super(TYPE, callTarget.returnStamp().getTrustedStamp());
        this.exceptionEdge = exceptionEdge;
        this.bci = bci;
        this.callTarget = callTarget;
        this.polymorphic = false;
        this.useForInlining = true;
        this.exceptionProbability = EXCEPTION_PROBA;
    }

    @Override
    protected void afterClone(Node other) {
        updateInliningLogAfterClone(other);
    }

    @Override
    public FixedNode asFixedNode() {
        return this;
    }

    public AbstractBeginNode exceptionEdge() {
        return exceptionEdge;
    }

    public void setExceptionEdge(AbstractBeginNode x) {
        updatePredecessor(exceptionEdge, x);
        exceptionEdge = x;
    }

    @Override
    public AbstractBeginNode next() {
        return next;
    }

    public void setNext(AbstractBeginNode x) {
        updatePredecessor(next, x);
        next = x;
    }

    @Override
    public CallTargetNode callTarget() {
        return callTarget;
    }

    void setCallTarget(CallTargetNode callTarget) {
        updateUsages(this.callTarget, callTarget);
        this.callTarget = callTarget;
    }

    public MethodCallTargetNode methodCallTarget() {
        return (MethodCallTargetNode) callTarget;
    }

    @Override
    public boolean isPolymorphic() {
        return polymorphic;
    }

    @Override
    public void setPolymorphic(boolean value) {
        this.polymorphic = value;
    }

    @Override
    public boolean useForInlining() {
        return useForInlining;
    }

    @Override
    public void setUseForInlining(boolean value) {
        this.useForInlining = value;
    }

    @Override
    public String toString(Verbosity verbosity) {
        if (verbosity == Verbosity.Long) {
            return super.toString(Verbosity.Short) + "(bci=" + bci() + ")";
        } else if (verbosity == Verbosity.Name) {
            return "Invoke#" + (callTarget == null ? "null" : callTarget().targetName());
        } else {
            return super.toString(verbosity);
        }
    }

    @Override
    public int bci() {
        return bci;
    }

    @Override
    public void setNext(FixedNode x) {
        if (x != null) {
            this.setNext(KillingBeginNode.begin(x, this.getKilledLocationIdentity()));
        } else {
            this.setNext(null);
        }
    }

    @Override
    public void lower(LoweringTool tool) {
        tool.getLowerer().lower(this, tool);
    }

    @Override
    public void generate(NodeLIRBuilderTool gen) {
        gen.emitInvoke(this);
    }

    @Override
    public FrameState stateAfter() {
        return stateAfter;
    }

    @Override
    public void setStateAfter(FrameState stateAfter) {
        updateUsages(this.stateAfter, stateAfter);
        this.stateAfter = stateAfter;
    }

    @Override
    public boolean hasSideEffect() {
        return true;
    }

    @Override
    public LocationIdentity getKilledLocationIdentity() {
        return LocationIdentity.any();
    }

    @Override
    public Map<Object, Object> getDebugProperties(Map<Object, Object> map) {
        Map<Object, Object> debugProperties = super.getDebugProperties(map);
        if (callTarget != null) {
            debugProperties.put("targetMethod", callTarget.targetName());
        }
        return debugProperties;
    }

    public void killExceptionEdge() {
        AbstractBeginNode edge = exceptionEdge();
        setExceptionEdge(null);
        GraphUtil.killCFG(edge);
    }

    @SuppressWarnings("try")
    public AbstractBeginNode killKillingBegin() {
        AbstractBeginNode begin = next();
        if (begin instanceof KillingBeginNode) {
            try (DebugCloseable position = begin.withNodeSourcePosition()) {
                AbstractBeginNode newBegin = new BeginNode();
                graph().addAfterFixed(begin, graph().add(newBegin));
                begin.replaceAtUsages(newBegin);
                graph().removeFixed(begin);
                return newBegin;
            }
        }
        return begin;
    }

    @Override
    public void replaceBci(int newBci) {
        assert BytecodeFrame.isPlaceholderBci(bci) && !BytecodeFrame.isPlaceholderBci(newBci) : "can only replace placeholder with better bci";
        bci = newBci;
    }

    @Override
    public double probability(AbstractBeginNode successor) {
        return successor == next ? 1 - exceptionProbability : exceptionProbability;
    }

    @Override
    public boolean canDeoptimize() {
        return true;
    }

    @Override
    public FrameState stateDuring() {
        return stateDuring;
    }

    @Override
    public void setStateDuring(FrameState stateDuring) {
        updateUsages(this.stateDuring, stateDuring);
        this.stateDuring = stateDuring;
    }

    @Override
    public AbstractBeginNode getPrimarySuccessor() {
        return this.next();
    }

    @Override
    public Stamp uncheckedStamp() {
        return this.callTarget.returnStamp().getUncheckedStamp();
    }

    @Override
    public void setClassInit(ValueNode classInit) {
        this.classInit = classInit;
        updateUsages(null, classInit);
    }

    @Override
    public ValueNode classInit() {
        return classInit;
    }

    @Override
    public boolean setProbability(AbstractBeginNode successor, double value) {
        // Cannot set probability for exception invokes.
        return false;
    }

    @Override
    public int getSuccessorCount() {
        return 2;
    }

    /**
     * Replaces this InvokeWithExceptionNode with a normal InvokeNode. Kills the exception dispatch
     * code.
     */
    public InvokeNode replaceWithInvoke() {
        InvokeNode newInvoke = graph().add(new InvokeNode(callTarget, bci, stamp, this.getKilledLocationIdentity()));
        newInvoke.setStateAfter(stateAfter);
        newInvoke.setStateDuring(stateDuring);
        AbstractBeginNode oldException = this.exceptionEdge;
        graph().replaceSplitWithFixed(this, newInvoke, this.next());
        GraphUtil.killCFG(oldException);
        return newInvoke;
    }
}
