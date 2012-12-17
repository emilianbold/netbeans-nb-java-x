/*
 * @test /nodynamiccopyright/
 * @library ..
 * @build DocLintTester
 * @run main DocLintTester -ref InvalidURI.out InvalidURI.java
 */

// tidy: Warning: <a> escaping malformed URI reference
// tidy: Warning: <.*> attribute ".*" lacks value

/**
 * <a href="abc">valid</a>
 * <a href="abc%20def">valid</a>
 * <a href="abc def">invalid</a>
 * <a href>no value</a>
 * <a href= >no value</a>
 * <a href="" >no value</a>
 */
public class InvalidURI { }
