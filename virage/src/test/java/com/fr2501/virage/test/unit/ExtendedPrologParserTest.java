package com.fr2501.virage.test.unit;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;

import com.fr2501.virage.prolog.ExtendedPrologParser;
import com.fr2501.virage.prolog.MalformedEplFileException;
import com.fr2501.virage.prolog.SimpleExtendedPrologParser;
import com.fr2501.virage.types.FrameworkRepresentation;

/**
 * Test suite for {@link ExtendedPrologParser}.
 *
 */
public class ExtendedPrologParserTest {
    /**
     * The logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(ExtendedPrologParserTest.class);

    /**
     * Tests loading a non-existing file.
     * @throws IOException if io fails
     * @throws MalformedEplFileException if the file is not valid (E)PL
     */
    @Test(expected = FileNotFoundException.class)
    public void loadNonExistingFile() throws IOException, MalformedEplFileException {
        LOGGER.info("loadNonExistingFile()");
        final ExtendedPrologParser parser = new SimpleExtendedPrologParser();

        parser.parseFramework(new File(""), false);
    }

    /**
     * Tests loading an invalid (E)PL file.
     * @throws IOException if io fails
     * @throws MalformedEplFileException if the file is not valid (E)PL
     */
    @Test(expected = MalformedEplFileException.class)
    public void loadInvalidFile() throws IOException, MalformedEplFileException {
        LOGGER.info("loadInvalidFile()");
        final ExtendedPrologParser parser = new SimpleExtendedPrologParser();

        parser.parseFramework(new File("src/test/resources/invalid_test.pl"), false);
    }

    /*
     * @Test This test now depends heavily depends on the config file, so it is no longer
     * meaningful.
     */
    /**
     * Tests the loading process of a valid EPL file.
     *
     * @throws IOException if file interaction fails
     * @throws MalformedEplFileException if the file violates the EPL format
     */
    public void loadValidFile() throws IOException, MalformedEplFileException {
        LOGGER.info("loadValidFile()");
        final ExtendedPrologParser parser = new SimpleExtendedPrologParser();

        final FrameworkRepresentation framework = parser
                .parseFramework(new File("src/test/resources/valid_test.pl"), false);

        // The first assertion now depends on a config file.
        // assertTrue(framework.getComponentTypes().size() == 3);
        assertTrue(framework.getComponents().size() == 3);
        assertTrue(framework.getComposableModules().size() == 0);
        assertTrue(framework.getProperties().size() == 3);
        assertTrue(framework.getCompositionRules().size() == 4);
    }

    /**
     * Tests loading a valid file.
     * @throws IOException if io fails
     * @throws MalformedEplFileException if the file is not valid (E)PL
     */
    @Test
    public void loadFrameworkFile() throws IOException, MalformedEplFileException {
        LOGGER.info("loadFrameworkFile()");
        final ExtendedPrologParser parser = new SimpleExtendedPrologParser();

        final FrameworkRepresentation framework = parser
                .parseFramework(new File("src/test/resources/framework.pl"), false);
        LOGGER.debug(framework.toString());
    }
}
