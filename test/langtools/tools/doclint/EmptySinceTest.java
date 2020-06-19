/*
 * @test /nodynamiccopyright/
 * @bug 8004832 8247815
 * @summary Add new doclint package
 * @modules jdk.compiler/com.sun.tools.doclint
 * @build DocLintTester
 * @run main DocLintTester -Xmsgs:-missing EmptySinceTest.java
 * @run main DocLintTester -Xmsgs:missing -ref EmptySinceTest.out EmptySinceTest.java
 */

/** . */
public class EmptySinceTest {
    /** @since */
    void emptySince() { }
}
