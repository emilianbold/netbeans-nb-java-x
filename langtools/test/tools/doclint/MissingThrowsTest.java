/*
 * @test /nodynamiccopyright/
 * @bug 8004832
 * @summary Add new doclint package
 * @build DocLintTester
 * @run main DocLintTester -Xmsgs:-missing MissingThrowsTest.java
 * @run main DocLintTester -Xmsgs:missing -ref MissingThrowsTest.out MissingThrowsTest.java
 */

/** */
public class MissingThrowsTest {
    /** */
    void missingThrows() throws Exception { }
}
