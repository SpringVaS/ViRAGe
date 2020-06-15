package com.fr2501.virage.core;

import java.io.File;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fr2501.virage.prolog.ExtendedPrologParser;
import com.fr2501.virage.prolog.MalformedEPLFileException;
import com.fr2501.virage.prolog.SimpleExtendedPrologParser;
import com.fr2501.virage.types.FrameworkRepresentation;

/**
 * 
 * The main application
 *
 */

public class VirageCore {
	private final static String _NAME = "ViRAGe";
	private final static String _VERSION = "0.0.1";
	private final static Logger logger = LogManager.getLogger(VirageCore.class.getName());
	
	private static ExtendedPrologParser extendedPrologParser;
	private static FrameworkRepresentation framework;
	private static VirageSearchManager manager;

    public static void main(String[] args) throws IOException, MalformedEPLFileException {
        logger.info("--- " + _NAME + " version " + _VERSION);
        logger.info("Initialising VirageCore.");
        
        extendedPrologParser = new SimpleExtendedPrologParser();
        framework = extendedPrologParser.parseFramework(new File("src/main/resources/framework.pl"));
        manager = new VirageSearchManager();
    }
}
