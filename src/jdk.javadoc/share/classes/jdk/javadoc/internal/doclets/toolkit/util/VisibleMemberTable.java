/*
 * Copyright (c) 2018, 2020, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
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

package jdk.javadoc.internal.doclets.toolkit.util;

import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Elements;
import javax.lang.model.util.SimpleElementVisitor14;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import jdk.javadoc.internal.doclets.toolkit.BaseConfiguration;
import jdk.javadoc.internal.doclets.toolkit.BaseOptions;
import jdk.javadoc.internal.doclets.toolkit.PropertyUtils;

/**
 * This class computes the main data structure for the doclet's
 * operations. Essentially, the implementation encapsulating the
 * javax.lang.models view of what can be documented about a
 * type element's members.
 * <p>
 * The general operations are as follows:
 * <p>
 * Members: these are the members from jx.l.m's view but
 * are structured along the kinds of this class.
 * <p>
 * Extra Members: these are members enclosed in an undocumented
 * package-private type element, and may not be linkable (or documented),
 * however, the members of such a type element may be documented, as if
 * declared in the sub type, only if the enclosing type is not being
 * documented by a filter such as -public, -protected, etc.
 * <p>
 * Visible Members: these are the members that are "visible"
 * and available and should be documented, in a type element.
 * <p>
 * The basic rule for computation: when considering a type element,
 * besides its immediate direct types and interfaces, the computation
 * should not expand to any other type in the inheritance hierarchy.
 * <p>
 * This table generates all the data structures it needs for each
 * type, as its own view, and will present some form of this to the
 * doclet as and when required to.
 *
 * <p><b>This is NOT part of any supported API.
 * If you write code that depends on this, you do so at your own risk.
 * This code and its internal interfaces are subject to change or
 * deletion without notice.</b>
 *
 */

public class VisibleMemberTable {

    public enum Kind {
        INNER_CLASSES,
        ENUM_CONSTANTS,
        FIELDS,
        CONSTRUCTORS,
        METHODS,
        ANNOTATION_TYPE_FIELDS,
        ANNOTATION_TYPE_MEMBER_OPTIONAL,
        ANNOTATION_TYPE_MEMBER_REQUIRED,
        PROPERTIES;

        public static final EnumSet<Kind> summarySet = EnumSet.range(INNER_CLASSES, METHODS);
        public static final EnumSet<Kind> detailSet = EnumSet.range(ENUM_CONSTANTS, METHODS);
    }

    final TypeElement te;
    final TypeElement parent;

    final BaseConfiguration config;
    final BaseOptions options;
    final Utils utils;
    final VisibleMemberCache mcache;

    private List<VisibleMemberTable> allSuperclasses;
    private List<VisibleMemberTable> allSuperinterfaces;
    private List<VisibleMemberTable> parents;


    private Map<Kind, List<Element>> extraMembers = new EnumMap<>(Kind.class);
    private Map<Kind, List<Element>> visibleMembers = null;
    private Map<ExecutableElement, PropertyMembers> propertyMap = new HashMap<>();

    // Keeps track of method overrides
    Map<ExecutableElement, OverridingMethodInfo> overriddenMethodTable
            = new LinkedHashMap<>();

    protected VisibleMemberTable(TypeElement typeElement, BaseConfiguration configuration,
                                 VisibleMemberCache mcache) {
        config = configuration;
        utils = configuration.utils;
        options = configuration.getOptions();
        te = typeElement;
        parent = utils.getSuperClass(te);
        this.mcache = mcache;
        allSuperclasses = new ArrayList<>();
        allSuperinterfaces = new ArrayList<>();
        parents = new ArrayList<>();
    }

    private synchronized void ensureInitialized() {
        if (visibleMembers != null)
            return;

        visibleMembers = new EnumMap<>(Kind.class);
        for (Kind kind : Kind.values()) {
            visibleMembers.put(kind, new ArrayList<>());
        }
        computeParents();
        computeVisibleMembers();
    }

    List<? extends Element> getExtraMembers(Kind kind) {
        ensureInitialized();
        return visibleMembers.getOrDefault(kind, Collections.emptyList());
    }

    List<VisibleMemberTable> getAllSuperclasses() {
        ensureInitialized();
        return allSuperclasses;
    }

    List<VisibleMemberTable> getAllSuperinterfaces() {
        ensureInitialized();
        return allSuperinterfaces;
    }

    /**
     * Returns a list of all visible enclosed members of a type element,
     * and inherited members.
     * <p>
     * Notes:
     * a. The list may or may not contain simple overridden methods.
     * A simple overridden method is one that overrides a super method
     * with no specification changes as indicated by the existence of a
     * sole &commat;inheritDoc or devoid of any API comments.
     * <p>
     * b.The list may contain (extra) members, inherited by inaccessible
     * super types, primarily package private types. These members are
     * required to be documented in the subtype when the super type is
     * not documented.
     *
     * @param kind the member kind
     * @return a list of all visible members
     */
    public List<? extends Element> getAllVisibleMembers(Kind kind) {
        ensureInitialized();
        return visibleMembers.getOrDefault(kind, Collections.emptyList());
    }

    /**
     * Returns a list of visible enclosed members of a specified kind,
     * filtered by the specified predicate.
     * @param kind the member kind
     * @param p the predicate used to filter the output
     * @return a list of visible enclosed members
     */
    public List<? extends Element> getVisibleMembers(Kind kind, Predicate<Element> p) {
        ensureInitialized();

        return visibleMembers.getOrDefault(kind, Collections.emptyList()).stream()
                .filter(p)
                .collect(Collectors.toList());
    }

    /**
     * Returns a list of all enclosed members including any extra members.
     * Typically called by various builders.
     *
     * @param kind the member kind
     * @return a list of visible enclosed members
     */
    public List<? extends Element> getVisibleMembers(Kind kind) {
        Predicate<Element> declaredAndLeafMembers = e -> {
            TypeElement encl = utils.getEnclosingTypeElement(e);
            return encl == te || utils.isUndocumentedEnclosure(encl);
        };
        return getVisibleMembers(kind, declaredAndLeafMembers);
    }

    /**
     * Returns a list of visible enclosed members of given kind,
     * declared in this type element, and does not include
     * any inherited members or extra members.
     *
     * @return a list of visible enclosed members in this type
     */
    public List<? extends Element> getMembers(Kind kind) {
        Predicate<Element> onlyLocallyDeclaredMembers = e -> utils.getEnclosingTypeElement(e) == te;
        return getVisibleMembers(kind, onlyLocallyDeclaredMembers);
    }

    /**
     * Returns the overridden method, if it is simply overriding or the
     * method is a member of a package private type, this method is
     * primarily used to determine the location of a possible comment.
     *
     * @param e the method to check
     * @return the method found or null
     */
    public ExecutableElement getOverriddenMethod(ExecutableElement e) {
        ensureInitialized();

        OverridingMethodInfo found = overriddenMethodTable.get(e);
        if (found != null
                && (found.simpleOverride || utils.isUndocumentedEnclosure(utils.getEnclosingTypeElement(e)))) {
            return found.overrider;
        }
        return null;
    }

    /**
     * Returns the simply overridden method.
     * @param e the method to check
     * @return the overridden method or null
     */
    public ExecutableElement getSimplyOverriddenMethod(ExecutableElement e) {
        ensureInitialized();

        OverridingMethodInfo found = overriddenMethodTable.get(e);
        if (found != null && found.simpleOverride) {
            return found.overrider;
        }
        return null;
    }

    /**
     * Returns a set of visible type elements in this type element's lineage.
     * <p>
     * This method returns the super-types in the inheritance
     * order C, B, A, j.l.O. The super-interfaces however are
     * alpha sorted and appended to the resulting set.
     *
     * @return the list of visible classes in this map.
     */
    public Set<TypeElement> getVisibleTypeElements() {
        ensureInitialized();
        Set<TypeElement> result = new LinkedHashSet<>();

        // Add this type element first.
        result.add(te);

        // Add the super classes.
        allSuperclasses.stream()
                .map(vmt -> vmt.te)
                .forEach(result::add);

        // ... and finally the sorted super interfaces.
        allSuperinterfaces.stream()
                .map(vmt -> vmt.te)
                .sorted(utils.makeGeneralPurposeComparator())
                .forEach(result::add);

        return result;
    }

    /**
     * Returns true if this table contains visible members.
     *
     * @return true if visible members are present.
     */
    public boolean hasVisibleMembers() {
        for (Kind kind : Kind.values()) {
            if (hasVisibleMembers(kind))
                return true;
        }
        return false;
    }

    /**
     * Returns true if this table contains visible members of
     * the specified kind, including inherited members.
     *
     * @return true if visible members are present.
     */
    public boolean hasVisibleMembers(Kind kind) {
        ensureInitialized();
        List<Element> elements = visibleMembers.get(kind);
        return elements != null && !elements.isEmpty();
    }

    /**
     * Returns the property field associated with the property method.
     * @param propertyMethod the identifying property method
     * @return the field or null if absent
     */
    public VariableElement getPropertyField(ExecutableElement propertyMethod) {
        ensureInitialized();
        PropertyMembers pm =  propertyMap.get(propertyMethod);
        return pm == null ? null : pm.field;
    }

    /**
     * Returns the getter method associated with the property method.
     * @param propertyMethod the identifying property method
     * @return the getter or null if absent
     */
    public ExecutableElement getPropertyGetter(ExecutableElement propertyMethod) {
        ensureInitialized();
        PropertyMembers pm =  propertyMap.get(propertyMethod);
        return pm == null ? null : pm.getter;
    }

    /**
     * Returns the setter method associated with the property method.
     * @param propertyMethod the identifying property method
     * @return the setter or null if absent
     */
    public ExecutableElement getPropertySetter(ExecutableElement propertyMethod) {
        ensureInitialized();
        PropertyMembers pm =  propertyMap.get(propertyMethod);
        return pm == null ? null : pm.setter;
    }

    private void computeParents() {
        for (TypeMirror intfType : te.getInterfaces()) {
            TypeElement intfc = utils.asTypeElement(intfType);
            if (intfc != null) {
                VisibleMemberTable vmt = mcache.getVisibleMemberTable(intfc);
                allSuperinterfaces.add(vmt);
                parents.add(vmt);
                allSuperinterfaces.addAll(vmt.getAllSuperinterfaces());
            }
        }

        if (parent != null) {
            VisibleMemberTable vmt = mcache.getVisibleMemberTable(parent);
            allSuperclasses.add(vmt);
            allSuperclasses.addAll(vmt.getAllSuperclasses());
            // Add direct super interfaces of a super class, if any.
            allSuperinterfaces.addAll(vmt.getAllSuperinterfaces());
            parents.add(vmt);
        }
    }

    private void computeVisibleMembers() {

        // Note: these have some baggage, and are redundant,
        // allow this to be GC'ed.
        LocalMemberTable lmt = new LocalMemberTable();

        for (Kind k : Kind.values()) {
            computeLeafMembers(lmt, k);
            computeVisibleMembers(lmt, k);
        }
        // All members have been computed, compute properties.
        computeVisibleProperties(lmt);
    }

    private void computeLeafMembers(LocalMemberTable lmt, Kind kind) {
        List<Element> list = new ArrayList<>();
        if (utils.isUndocumentedEnclosure(te)) {
            list.addAll(lmt.getOrderedMembers(kind));
        }
        parents.forEach(pvmt -> {
            list.addAll(pvmt.getExtraMembers(kind));
        });
        extraMembers.put(kind, Collections.unmodifiableList(list));
    }

    void computeVisibleMembers(LocalMemberTable lmt, Kind kind) {
        switch (kind) {
            case FIELDS: case INNER_CLASSES:
                computeVisibleFieldsAndInnerClasses(lmt, kind);
                return;

            case METHODS:
                computeVisibleMethods(lmt);
                return;

            // Defer properties related computations for later.
            case PROPERTIES:
                return;

            default:
                List<Element> list = lmt.getOrderedMembers(kind).stream()
                        .filter(this::mustDocument)
                        .collect(Collectors.toList());
                visibleMembers.put(kind, Collections.unmodifiableList(list));
                break;
        }
    }

    private boolean mustDocument(Element e) {
        return !utils.hasHiddenTag(e) && utils.shouldDocument(e);
    }

    private boolean allowInheritedMembers(Element e, Kind kind, LocalMemberTable lmt) {
        return isInherited(e) && !isMemberHidden(e, kind, lmt);
    }

    private boolean isInherited(Element e) {
        if (utils.isPrivate(e))
            return false;

        if (utils.isPackagePrivate(e))
            // Allowed iff this type-element is in the same package as the element
            return utils.containingPackage(e).equals(utils.containingPackage(te));

        return true;
    }

    private boolean isMemberHidden(Element inheritedMember, Kind kind, LocalMemberTable lmt) {
        Elements elementUtils = config.docEnv.getElementUtils();
        switch(kind) {
            default:
                List<Element> list = lmt.getMembers(inheritedMember, kind);
                if (list.isEmpty())
                    return false;
                return elementUtils.hides(list.get(0), inheritedMember);
            case METHODS: case CONSTRUCTORS: // Handled elsewhere.
                throw new IllegalArgumentException("incorrect kind");
        }
    }

    private void computeVisibleFieldsAndInnerClasses(LocalMemberTable lmt, Kind kind) {
        Set<Element> result = new LinkedHashSet<>();
        for (VisibleMemberTable pvmt : parents) {
            result.addAll(pvmt.getExtraMembers(kind));
            result.addAll(pvmt.getAllVisibleMembers(kind));
        }

        // Filter out members in the inherited list that are hidden
        // by this type or should not be inherited at all.
        List<Element> list = result.stream()
                .filter(e -> allowInheritedMembers(e, kind, lmt)).collect(Collectors.toList());

        // Prefix local results first
        list.addAll(0, lmt.getOrderedMembers(kind));

        // Filter out elements that should not be documented
        list = list.stream()
                .filter(this::mustDocument)
                .collect(Collectors.toList());

        visibleMembers.put(kind, Collections.unmodifiableList(list));
    }

    private void computeVisibleMethods(LocalMemberTable lmt) {
        Set<Element> inheritedMethods = new LinkedHashSet<>();
        Map<ExecutableElement, List<ExecutableElement>> overriddenByTable = new HashMap<>();
        for (VisibleMemberTable pvmt : parents) {
            // Merge the lineage overrides into local table
            pvmt.overriddenMethodTable.entrySet().forEach(e -> {
                OverridingMethodInfo p = e.getValue();
                if (!p.simpleOverride) { // consider only real overrides
                    List<ExecutableElement> list = overriddenByTable.computeIfAbsent(p.overrider,
                            k -> new ArrayList<>());
                    list.add(e.getKey());
                }
            });
            inheritedMethods.addAll(pvmt.getAllVisibleMembers(Kind.METHODS));

            // Copy the extra members (if any) from the lineage.
            if (!utils.shouldDocument(pvmt.te)) {
                List<? extends Element> extraMethods = pvmt.getExtraMembers(Kind.METHODS);

                if (lmt.getOrderedMembers(Kind.METHODS).isEmpty()) {
                    inheritedMethods.addAll(extraMethods);
                    continue;
                }

                // Check if an extra-method ought to percolate through.
                for (Element extraMethod : extraMethods) {
                    boolean found = false;

                    List<Element> lmethods = lmt.getMembers(extraMethod, Kind.METHODS);
                    for (Element lmethod : lmethods) {
                        ExecutableElement method = (ExecutableElement)lmethod;
                        found = utils.elementUtils.overrides(method,
                                (ExecutableElement)extraMethod, te);
                        if (found)
                            break;
                    }
                    if (!found)
                        inheritedMethods.add(extraMethod);
                }
            }
        }

        // Filter out inherited methods that:
        // a. cannot override (private instance members)
        // b. are overridden and should not be visible in this type
        // c. are hidden in the type being considered
        // see allowInheritedMethods, which performs the above actions
        List<Element> list = inheritedMethods.stream()
                .filter(e -> allowInheritedMethods((ExecutableElement)e, overriddenByTable, lmt))
                .collect(Collectors.toList());

        // Filter out the local methods, that do not override or simply
        // overrides a super method, or those methods that should not
        // be visible.
        Predicate<ExecutableElement> isVisible = m -> {
            OverridingMethodInfo p = overriddenMethodTable.getOrDefault(m, null);
            return p == null || !p.simpleOverride;
        };
        List<Element> mlist = lmt.getOrderedMembers(Kind.METHODS);
        List<Element> llist = mlist.stream()
                .map(m -> (ExecutableElement)m)
                .filter(isVisible)
                .collect(Collectors.toList());

        // Merge the above lists, making sure the local methods precede
        // the others
        list.addAll(0, llist);

        // Final filtration of elements
        list = list.stream()
                .filter(this::mustDocument)
                .collect(Collectors.toList());

        visibleMembers.put(Kind.METHODS, Collections.unmodifiableList(list));

        // Copy over overridden tables from the lineage, and finish up.
        for (VisibleMemberTable pvmt : parents) {
            overriddenMethodTable.putAll(pvmt.overriddenMethodTable);
        }
        overriddenMethodTable = Collections.unmodifiableMap(overriddenMethodTable);
    }

    boolean isEnclosureInterface(Element e) {
        TypeElement enclosing = utils.getEnclosingTypeElement(e);
        return utils.isInterface(enclosing);
    }

    boolean allowInheritedMethods(ExecutableElement inheritedMethod,
                                  Map<ExecutableElement, List<ExecutableElement>> inheritedOverriddenTable,
                                  LocalMemberTable lmt) {
        if (!isInherited(inheritedMethod))
            return false;

        final boolean haveStatic = utils.isStatic(inheritedMethod);
        final boolean inInterface = isEnclosureInterface(inheritedMethod);

        // Static methods in interfaces are never documented.
        if (haveStatic && inInterface) {
            return false;
        }

        // Multiple-Inheritance: remove the interface method that may have
        // been overridden by another interface method in the hierarchy
        //
        // Note: The following approach is very simplistic and is compatible
        // with old VMM. A future enhancement, may include a contention breaker,
        // to correctly eliminate those methods that are merely definitions
        // in favor of concrete overriding methods, for instance those that have
        // API documentation and are not abstract OR default methods.
        if (inInterface) {
            List<ExecutableElement> list = inheritedOverriddenTable.get(inheritedMethod);
            if (list != null) {
                boolean found = list.stream()
                        .anyMatch(this::isEnclosureInterface);
                if (found)
                    return false;
            }
        }

        Elements elementUtils = config.docEnv.getElementUtils();

        // Check the local methods in this type.
        List<Element> lMethods = lmt.getMembers(inheritedMethod, Kind.METHODS);
        for (Element le : lMethods) {
            ExecutableElement lMethod = (ExecutableElement) le;
            // Ignore private methods or those methods marked with
            // a "hidden" tag.
            if (utils.isPrivate(lMethod))
                continue;

            // Remove methods that are "hidden", in JLS terms.
            if (haveStatic && utils.isStatic(lMethod) &&
                    elementUtils.hides(lMethod, inheritedMethod)) {
                return false;
            }

            // Check for overriding methods.
            if (elementUtils.overrides(lMethod, inheritedMethod,
                    utils.getEnclosingTypeElement(lMethod))) {

                // Disallow package-private super methods to leak in
                TypeElement encl = utils.getEnclosingTypeElement(inheritedMethod);
                if (utils.isUndocumentedEnclosure(encl)) {
                    overriddenMethodTable.computeIfAbsent(lMethod,
                            l -> new OverridingMethodInfo(inheritedMethod, false));
                    return false;
                }

                TypeMirror inheritedMethodReturn = inheritedMethod.getReturnType();
                TypeMirror lMethodReturn = lMethod.getReturnType();
                boolean covariantReturn =
                        lMethodReturn.getKind() == TypeKind.DECLARED
                        && inheritedMethodReturn.getKind() == TypeKind.DECLARED
                        && !utils.typeUtils.isSameType(lMethodReturn, inheritedMethodReturn)
                        && utils.typeUtils.isSubtype(lMethodReturn, inheritedMethodReturn);
                boolean simpleOverride = covariantReturn ? false : utils.isSimpleOverride(lMethod);
                overriddenMethodTable.computeIfAbsent(lMethod,
                        l -> new OverridingMethodInfo(inheritedMethod, simpleOverride));
                return simpleOverride;
            }
        }
        return true;
    }

    /*
     * This class encapsulates the details of local members, orderedMembers
     * contains the members in the declaration order, additionally a
     * HashMap is maintained for performance optimization to lookup
     * members. As a future enhancement is perhaps to consolidate the ordering
     * into a Map, capturing the insertion order, thereby eliminating an
     * ordered list.
     */
    class LocalMemberTable {

        // Maintains declaration order
        private final Map<Kind, List<Element>> orderedMembers;

        // Performance optimization
        private final Map<Kind, Map<String, List<Element>>> memberMap;

        LocalMemberTable() {
            orderedMembers = new EnumMap<>(Kind.class);
            memberMap = new EnumMap<>(Kind.class);

            List<? extends Element> elements = te.getEnclosedElements();
            for (Element e : elements) {
                if (options.noDeprecated() && utils.isDeprecated(e)) {
                    continue;
                }
                switch (e.getKind()) {
                    case CLASS:
                    case INTERFACE:
                    case ENUM:
                    case ANNOTATION_TYPE:
                    case RECORD:
                        addMember(e, Kind.INNER_CLASSES);
                        break;
                    case FIELD:
                        addMember(e, Kind.FIELDS);
                        addMember(e, Kind.ANNOTATION_TYPE_FIELDS);
                        break;
                    case METHOD:
                        ExecutableElement ee = (ExecutableElement)e;
                        if (utils.isAnnotationType(te)) {
                            addMember(e, ee.getDefaultValue() == null
                                    ? Kind.ANNOTATION_TYPE_MEMBER_REQUIRED
                                    : Kind.ANNOTATION_TYPE_MEMBER_OPTIONAL);
                        }
                        addMember(e, Kind.METHODS);
                        break;
                    case CONSTRUCTOR:
                            addMember(e, Kind.CONSTRUCTORS);
                        break;
                    case ENUM_CONSTANT:
                        addMember(e, Kind.ENUM_CONSTANTS);
                        break;
                }
            }

            // Freeze the data structures
            for (Kind kind : Kind.values()) {
                orderedMembers.computeIfPresent(kind, (k, v) -> Collections.unmodifiableList(v));
                orderedMembers.computeIfAbsent(kind, t -> Collections.emptyList());

                memberMap.computeIfPresent(kind, (k, v) -> Collections.unmodifiableMap(v));
                memberMap.computeIfAbsent(kind, t -> Collections.emptyMap());
            }
        }

        @SuppressWarnings("preview")
        String getMemberKey(Element e) {
            return new SimpleElementVisitor14<String, Void>() {
                @Override
                public String visitExecutable(ExecutableElement e, Void aVoid) {
                    return e.getSimpleName() + ":" + e.getParameters().size();
                }

                @Override
                protected String defaultAction(Element e, Void aVoid) {
                    return e.getSimpleName().toString();
                }
            }.visit(e);
        }

        void addMember(Element e, Kind kind) {
            List<Element> list = orderedMembers.computeIfAbsent(kind, k -> new ArrayList<>());
            list.add(e);

            Map<String, List<Element>> map = memberMap.computeIfAbsent(kind, k -> new HashMap<>());
            list = map.computeIfAbsent(getMemberKey(e), l -> new ArrayList<>());
            list.add(e);
        }

        List<Element> getOrderedMembers(Kind kind) {
            return orderedMembers.get(kind);
        }

        List<Element> getMembers(Element e, Kind kind) {
            String key = getMemberKey(e);
            return getMembers(key, kind);
        }

        List<Element> getMembers(String key, Kind kind) {
            Map <String, List<Element>> map = memberMap.get(kind);
            return map.getOrDefault(key, Collections.emptyList());
        }

        List<Element> getPropertyMethods(String methodName, int argcount) {
            return getMembers(methodName + ":" + argcount, Kind.METHODS).stream()
                    .filter(m -> (utils.isPublic(m) || utils.isProtected(m)))
                    .collect(Collectors.toList());
        }
    }

    /**
     * The properties triad for a property method.
     */
    static class PropertyMembers {
        final VariableElement field;
        final ExecutableElement getter;
        final ExecutableElement setter;

        PropertyMembers(VariableElement field, ExecutableElement getter, ExecutableElement setter) {
            this.field = field;
            this.getter = getter;
            this.setter = setter;
        }

        public String toString() {
            return ("field: " + field + ", getter: " + getter + ", setter: " + setter);
        }
    }

    /*
     * JavaFX convention notes.
     * A JavaFX property-method is a method, which ends with "Property" in
     * its name, takes no parameters and typically returns a subtype of javafx.beans.
     * ReadOnlyProperty, in the strictest sense. However, it may not always
     * be possible for the doclet to have access to j.b.ReadOnlyProperty,
     * for this reason the strict check is disabled via an undocumented flag.
     *
     * Note, a method should not be considered as a property-method,
     * if it satisfied the previously stated conditions AND if the
     * method begins with "set", "get" or "is".
     *
     * Supposing we have  {@code BooleanProperty acmeProperty()}, then the
     * property-name  is "acme".
     *
     * Property field, one may or may not exist and could be private, and
     * should match the property-method.
     *
     * A property-setter is a method starting with "set", and the
     * first character of the upper-cased starting character of the property name, the
     * method must take 1 argument and must return a <code>void</code>.
     *
     * Using the above example {@code void setAcme(Something s)} can be
     * considered as a property-setter of the property "acme".
     *
     * A property-getter is a method  starting with "get" and the first character
     * upper-cased property-name, having no parameters. A method that does not take any
     * parameters and starting with "is" and an upper-cased property-name,
     * returning a primitive type boolean or BooleanProperty can also be
     * considered as a getter, however there must be only one getter for every property.
     *
     * For example {@code Object getAcme()} is a property-getter, and
     * {@code boolean isFoo()}
     */
    private void computeVisibleProperties(LocalMemberTable lmt) {
        if (!options.javafx())
            return;

        PropertyUtils pUtils = config.propertyUtils;
        List<ExecutableElement> list = visibleMembers.getOrDefault(Kind.METHODS, Collections.emptyList())
                .stream()
                .map(m -> (ExecutableElement)m)
                .filter(pUtils::isPropertyMethod)
                .collect(Collectors.toList());

        visibleMembers.put(Kind.PROPERTIES, Collections.unmodifiableList(list));

        List<ExecutableElement> propertyMethods = list.stream()
                .filter(e -> utils.getEnclosingTypeElement(e) == te)
                .collect(Collectors.toList());

        // Compute additional properties related sundries.
        for (ExecutableElement propertyMethod : propertyMethods) {
            String baseName = pUtils.getBaseName(propertyMethod);
            List<Element> flist = lmt.getMembers(baseName, Kind.FIELDS);
            Element field = flist.isEmpty() ? null : flist.get(0);

            Element getter = null, setter = null;
            List<Element> found = lmt.getPropertyMethods(pUtils.getGetName(propertyMethod), 0);
            if (!found.isEmpty()) {
                // Getters have zero params, no overloads! pick the first.
                getter = found.get(0);
            }
            if (getter == null) {
                // Check if isProperty methods are present ?
                found = lmt.getPropertyMethods(pUtils.getIsName(propertyMethod), 0);
                if (!found.isEmpty()) {
                    String propertyTypeName = propertyMethod.getReturnType().toString();
                    // Check if the return type of property method matches an isProperty method.
                    if (pUtils.hasIsMethod(propertyMethod)) {
                        // Getters have zero params, no overloads!, pick the first.
                        getter = found.get(0);
                    }
                }
            }
            found = lmt.getPropertyMethods(pUtils.getSetName(propertyMethod), 1);
            if (found != null) {
                for (Element e : found) {
                    if (pUtils.isValidSetterMethod((ExecutableElement)e)) {
                        setter = e;
                        break;
                    }
                }
            }

            propertyMap.put(propertyMethod, new PropertyMembers((VariableElement)field,
                    (ExecutableElement)getter, (ExecutableElement)setter));

            // Debugging purposes
            // System.out.println("te: " + te + ": " + utils.getEnclosingTypeElement(propertyMethod) +
            //        ":" + propertyMethod.toString() + "->" + propertyMap.get(propertyMethod));
        }
    }


    // Future cleanups

    Map<ExecutableElement, SoftReference<ImplementedMethods>> implementMethodsFinders = new HashMap<>();

    private ImplementedMethods getImplementedMethodsFinder(ExecutableElement method) {
        SoftReference<ImplementedMethods> imf = implementMethodsFinders.get(method);
        // IMF does not exist or referent was gc'ed away ?
        if (imf == null || imf.get() == null) {
            imf = new SoftReference<>(new ImplementedMethods(method));
            implementMethodsFinders.put(method, imf);
        }
        return imf.get();
    }

    public List<ExecutableElement> getImplementedMethods(ExecutableElement method) {
        ImplementedMethods imf = getImplementedMethodsFinder(method);
        return imf.getImplementedMethods().stream()
                .filter(m -> getSimplyOverriddenMethod(m) == null)
                .collect(Collectors.toList());
    }

    public TypeMirror getImplementedMethodHolder(ExecutableElement method,
                                                 ExecutableElement implementedMethod) {
        ImplementedMethods imf = getImplementedMethodsFinder(method);
        return imf.getMethodHolder(implementedMethod);
    }

    private class ImplementedMethods {

        private final Map<ExecutableElement, TypeMirror> interfaces = new HashMap<>();
        private final List<ExecutableElement> methlist = new ArrayList<>();
        private final TypeElement typeElement;
        private final ExecutableElement method;

        public ImplementedMethods(ExecutableElement method) {
            this.method = method;
            typeElement = utils.getEnclosingTypeElement(method);
            Set<TypeMirror> intfacs = utils.getAllInterfaces(typeElement);
            /*
             * Search for the method in the list of interfaces. If found check if it is
             * overridden by any other subinterface method which this class
             * implements. If it is not overridden, add it in the method list.
             * Do this recursively for all the extended interfaces for each interface
             * from the list.
             */
            for (TypeMirror interfaceType : intfacs) {
                ExecutableElement found = utils.findMethod(utils.asTypeElement(interfaceType), method);
                if (found != null) {
                    removeOverriddenMethod(found);
                    if (!overridingMethodFound(found)) {
                        methlist.add(found);
                        interfaces.put(found, interfaceType);
                    }
                }
            }
        }

        /**
         * Return the list of interface methods which the method passed in the
         * constructor is implementing. The search/build order is as follows:
         * <pre>
         * 1. Search in all the immediate interfaces which this method's class is
         *    implementing. Do it recursively for the superinterfaces as well.
         * 2. Traverse all the superclasses and search recursively in the
         *    interfaces which those superclasses implement.
         *</pre>
         *
         * @return SortedSet<ExecutableElement> of implemented methods.
         */
        List<ExecutableElement> getImplementedMethods() {
            return methlist;
        }

        TypeMirror getMethodHolder(ExecutableElement ee) {
            return interfaces.get(ee);
        }

        /**
         * Search in the method list and check if it contains a method which
         * is overridden by the method as parameter.  If found, remove the
         * overridden method from the method list.
         *
         * @param method Is this method overriding a method in the method list.
         */
        private void removeOverriddenMethod(ExecutableElement method) {
            TypeElement overriddenClass = utils.overriddenClass(method);
            if (overriddenClass != null) {
                for (int i = 0; i < methlist.size(); i++) {
                    TypeElement te = utils.getEnclosingTypeElement(methlist.get(i));
                    if (te == overriddenClass || utils.isSubclassOf(overriddenClass, te)) {
                        methlist.remove(i);  // remove overridden method
                        return;
                    }
                }
            }
        }

        /**
         * Search in the already found methods' list and check if it contains
         * a method which is overriding the method parameter or is the method
         * parameter itself.
         *
         * @param method method to be searched
         */
        private boolean overridingMethodFound(ExecutableElement method) {
            TypeElement containingClass = utils.getEnclosingTypeElement(method);
            for (ExecutableElement listmethod : methlist) {
                if (containingClass == utils.getEnclosingTypeElement(listmethod)) {
                    // it's the same method.
                    return true;
                }
                TypeElement te = utils.overriddenClass(listmethod);
                if (te == null) {
                    continue;
                }
                if (te == containingClass || utils.isSubclassOf(te, containingClass)) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * A simple container to encapsulate an overriding method
     * and the type of override.
     */
    static class OverridingMethodInfo {
        final ExecutableElement overrider;
        final boolean simpleOverride;

        public OverridingMethodInfo(ExecutableElement overrider, boolean simpleOverride) {
            this.overrider = overrider;
            this.simpleOverride = simpleOverride;
        }

        @Override
        public String toString() {
            return "OverridingMethodInfo[" + overrider + ",simple:" + simpleOverride + "]";
        }
    }
}
