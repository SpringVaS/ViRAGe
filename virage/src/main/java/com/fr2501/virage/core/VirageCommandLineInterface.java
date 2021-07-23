package com.fr2501.virage.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fr2501.util.StringUtils;
import com.fr2501.virage.jobs.VirageAnalyzeJob;
import com.fr2501.virage.jobs.VirageDummyJob;
import com.fr2501.virage.jobs.VirageExitJob;
import com.fr2501.virage.jobs.VirageExtractJob;
import com.fr2501.virage.jobs.VirageGenerateCCodeJob;
import com.fr2501.virage.jobs.VirageGenerateJob;
import com.fr2501.virage.jobs.VirageIsabelleGenerateJob;
import com.fr2501.virage.jobs.VirageIsabelleGenerateScalaJob;
import com.fr2501.virage.jobs.VirageIsabelleVerifyJob;
import com.fr2501.virage.jobs.VirageJob;
import com.fr2501.virage.jobs.VirageJobState;
import com.fr2501.virage.jobs.VirageParseJob;
import com.fr2501.virage.jobs.VirageProveJob;
import com.fr2501.virage.prolog.JplFacade;
import com.fr2501.virage.types.Component;
import com.fr2501.virage.types.ComposableModule;
import com.fr2501.virage.types.CompositionProof;
import com.fr2501.virage.types.CompositionalStructure;
import com.fr2501.virage.types.DecompositionTree;
import com.fr2501.virage.types.ExternalSoftwareUnavailableException;
import com.fr2501.virage.types.FrameworkRepresentation;
import com.fr2501.virage.types.InvalidConfigVersionException;
import com.fr2501.virage.types.Property;

/**
 * A simple command line interface for ViRAGe.
 *
 */
public final class VirageCommandLineInterface implements VirageUserInterface {
    /**
     * The logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(VirageCommandLineInterface.class);

    /**
     * The separator.
     */
    private static final String SEPARATOR =
            "###########################################################";
    /**
     * The banner.
     */
    private static final String BANNER = "#\n"
            + "# Y88b      / ,e, 888~-_        e       e88~~\\           \n"
            + "#  Y88b    /   \"  888   \\      d8b     d888      e88~~8e \n"
            + "#   Y88b  /   888 888    |    /Y88b    8888 __  d888  88b\n"
            + "#    Y888/    888 888   /    /  Y88b   8888   | 8888__888\n"
            + "#     Y8/     888 888_-~    /____Y88b  Y888   | Y888    ,\n"
            + "#      Y      888 888 ~-_  /      Y88b  \"88__/   \"88___/ \n#";

    /**
     * A Scanner to read input.
     */
    private final Scanner scanner;
    /**
     * The core this UI is attached to.
     */
    private final VirageCore core;

    /**
     * Writer to display output.
     */
    private final Writer outputWriter;

    /**
     * This UI's thread.
     */
    private Thread thread;

    /**
     * Simple constructor.
     *
     * @param core the ViRAGe core this UI will be attached to.
     */
    protected VirageCommandLineInterface(final VirageCore core) {
        this.outputWriter = new BufferedWriter(new OutputStreamWriter(System.out), 256);

        LOGGER.info("Initialising VirageCommandLineInterface.");

        this.scanner = new Scanner(System.in);
        this.core = core;

        this.printSeparator(); /* ----- */

        this.printBanner();

        this.printSeparator(); /* ----- */

        this.checkSettingsCompatibility();
        this.printTimestamp();

        this.printSeparator(); /* ----- */

        this.printPurpose();

        this.printSeparator(); /* ----- */

        this.checkEnvironment();

        this.printSeparator(); /* ----- */

        this.printSettings();

        this.printSeparator(); /* ----- */
    }

    private String addQuotationsToPath(final String pathString) {
        return "\'" + pathString + "\'";
    }

    private void checkEnvironment() {
        this.displayMessage("# " + ConfigReader.getInstance().checkAvailabilityAndGetVersions()
                .replace(System.lineSeparator(), "\n# "));

        boolean unsafeState = false;
        try {
            @SuppressWarnings("unused")
            final JplFacade facade = new JplFacade();
        } catch (final ExternalSoftwareUnavailableException e) {
            this.displayError(
                    "A required SWI-Prolog library (\"libswipl.so\") could not be located.");

            String newValue;
            try {
                while (true) {
                    newValue = this.requestString("Please input the path to libswipl.so. "
                            + "For your setup of SWI-Prolog, typical values are "
                            + this.addQuotationsToPath("/usr/lib/libswipl.so") + "or "
                            + this.addQuotationsToPath(
                                    ConfigReader.getInstance().getSwiplLib() + "libswipl.so")
                            + ", but this might differ on your system.");

                    if (!newValue.isEmpty()) {
                        final File file = new File(newValue);
                        if (!file.exists()) {
                            this.displayError("This file does not exist. Please try again.");
                            continue;
                        }

                        if (!newValue.endsWith("libswipl.so")) {
                            this.displayError("This is not \"libswipl.so\". Please try again.");
                            continue;
                        }

                        ConfigReader.getInstance().updateValueForLibswiplPath(newValue);
                        break;
                    }
                }
            } catch (final ExternalSoftwareUnavailableException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }

            unsafeState = true;
        } catch (final UnsatisfiedLinkError e) {
            this.displayError("The SWI-Prolog library directory could not be located. "
                    + "This directory must contain the library \"libjpl.so\", otherwise "
                    + "ViRAGe will not work properly.");

            String newValue;
            try {
                while (true) {
                    newValue = this.requestString("Please input the path to the SWI-Prolog "
                            + "library directory. "
                            + "For your setup of SWI-Prolog, the typical value is "
                            + this.addQuotationsToPath(ConfigReader.getInstance().getSwiplLib())
                            + ", but this might differ on your system.");

                    if (!newValue.isEmpty()) {
                        final File file = new File(newValue);
                        if (!file.exists()) {
                            this.displayError("This directory does not exist. Please try again.");
                            continue;
                        } else if (!file.isDirectory()) {
                            this.displayError("This path is not a directory. Please try again.");
                            continue;
                        } else if (!(new File(newValue + File.separator + "libjpl.so").exists())) {
                            this.displayError("This directory does not contain \"libjpl.so\". "
                                    + "You either supplied the wrong directory, "
                                    + "or JPL is not installed.");
                            continue;
                        }

                        ConfigReader.getInstance().updateValueForSwiPrologLibrariesPath(newValue);
                        break;
                    }
                }
            } catch (final ExternalSoftwareUnavailableException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }

            unsafeState = true;
        }

        if (unsafeState) {
            this.displayMessage("A restart is required for the changes to take effect. "
                    + "Please restart ViRAGe after it terminated.");
            System.exit(0);
        }
    }

    private void checkSettingsCompatibility() {
        try {
            ConfigReader.getInstance().readConfigFile(false);
        } catch (final InvalidConfigVersionException e) {
            this.displayMessage("# " + e.getMessage());
            if (this.requestConfirmation(
                    "# ViRAGe can replace the settings file with an up-to-date one. "
                            + "Your old settings file will be moved to \"old_settings\", "
                            + "and as many settings "
                            + "as possible will be transferred. "
                            + "Do you want to let ViRAGe perform this operation?")) {
                try {
                    try {
                        ConfigReader.getInstance().readConfigFile(true);
                    } catch (final InvalidConfigVersionException e1) {
                        // NO-OP, this cannot happen.
                        // readConfigFile(true) overwrites the old config.
                    }
                } catch (final IOException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }
            } else {
                this.displayMessage("# ViRAGe is in an unsafe state as you "
                        + "loaded an outdated configuration.");

                if (!this.requestConfirmation("Do you want to continue anyways?")) {
                    this.core.submit(new VirageExitJob(this, 0));
                }
            }

        } catch (final IOException e) {
            // TODO
            e.printStackTrace();
        }
    }

    @Override
    public int chooseAlternative(final String message, final List<?> alternatives) {
        this.displayMessage(message + System.lineSeparator());

        for (int i = 0; i < alternatives.size(); i++) {
            this.displayMessage("[" + i + "] " + alternatives.get(i).toString());
        }

        int res;
        while (true) {
            final String input = this.requestString("Please enter a number between 0 and "
                    + (alternatives.size() - 1) + " or leave empty to choose none of the above.");

            if (input.isEmpty()) {
                return -1;
            }

            try {
                res = Integer.parseInt(input);

                if (0 <= res && res < alternatives.size()) {
                    return res;
                }
            } catch (final NumberFormatException e) {
                // NO-OP
            }

            this.displayError("Please try again.");
        }
    }

    private VirageAnalyzeJob createAnalysisQuery() {
        final String composition = this.requestCompositionString();

        final String propertyString = this.requestPropertyString();

        final List<String> properties = StringUtils.separate(",", propertyString);

        final VirageAnalyzeJob res = new VirageAnalyzeJob(this, composition, properties);
        return res;
    }

    private VirageGenerateCCodeJob createCCodeGenerationQuery() {
        final String composition = this.requestCompositionString();

        final VirageGenerateCCodeJob res = new VirageGenerateCCodeJob(this, composition);
        return res;
    }

    private VirageIsabelleGenerateScalaJob createCodeGenerationQuery() {
        final String composition = this.requestCompositionString();

        final VirageIsabelleGenerateScalaJob res = new VirageIsabelleGenerateScalaJob(this,
                composition);
        return res;
    }

    private VirageGenerateJob createGenerationQuery() {
        final String propertyString = this.requestPropertyString();

        final List<String> properties = StringUtils.separate(",", propertyString);

        final VirageGenerateJob res = new VirageGenerateJob(this, properties);
        return res;
    }

    private VirageJob<?> createIsabelleQuery() {
        final String composition = this.requestCompositionString();

        final String propertyString = this.requestPropertyString();

        final String defaultPath = ConfigReader.getInstance().getDefaultOutputPath();
        String outputPath = this.requestString("Please specify a directory for the "
                + "generated theory file. (Press ENTER for default: "
                + this.addQuotationsToPath(defaultPath) + ")");
        if (outputPath.isEmpty()) {
            outputPath = defaultPath;
        }

        boolean verify = true;
        while (true) {
            if (this.requestConfirmation(
                    "Shall the resulting theory file be verified automatically?")) {
                break;
            } else {
                verify = false;
                break;
            }
        }

        final VirageProveJob proveJob = this.createProofQuery(composition, propertyString);
        this.core.submit(proveJob);
        proveJob.waitFor();

        if (proveJob.getState() == VirageJobState.FAILED) {
            LOGGER.warn("Proving the given claims failed.");
            return new VirageDummyJob(this);
        }

        final List<List<CompositionProof>> proofLists = proveJob.getResult();
        List<CompositionProof> bestProof = new LinkedList<CompositionProof>();
        for (final List<CompositionProof> proof : proofLists) {
            if (proof.size() > bestProof.size()) {
                bestProof = proof;
            }
        }

        final VirageIsabelleGenerateJob generateJob = new VirageIsabelleGenerateJob(this,
                composition, bestProof, outputPath);
        if (!verify) {
            return generateJob;
        }
        this.core.submit(generateJob);
        generateJob.waitFor();

        final VirageIsabelleVerifyJob verifyJob = new VirageIsabelleVerifyJob(this,
                generateJob.getResult());
        return verifyJob;
    }

    private VirageProveJob createProofQuery() {
        final String composition = this.requestCompositionString();

        final String propertyString = this.requestPropertyString();

        return this.createProofQuery(composition, propertyString);
    }

    private VirageProveJob createProofQuery(final String composition, final String propertyString) {
        final List<String> properties = StringUtils.separate(",", propertyString);

        final VirageProveJob res = new VirageProveJob(this, composition, properties);
        return res;
    }

    @Override
    public void displayError(final String message) {
        System.err.println(message);
    }

    /**
     * Display help from resources/help.txt.
     */
    public void displayHelp() {
        final InputStream helpStream = this.getClass().getClassLoader()
                .getResourceAsStream("help.txt");
        final StringWriter writer = new StringWriter();
        try {
            IOUtils.copy(helpStream, writer, StandardCharsets.UTF_8);
        } catch (final IOException e) {
            LOGGER.error("Something went wrong.", e);
            e.printStackTrace();
        }
        this.displayMessage(writer.toString());

        this.requestString("Press ENTER to leave help and return to previous state.");
    }

    private void displayInputMarker() {
        // Set background to white and text to black to signal input opportunity.
        // ">" would be nicer, but this appears to conflict with mvn exec:exec,
        // see https://github.com/mojohaus/exec-maven-plugin/issues/159.
        try {
            this.outputWriter.append("? \033[1A\n");
            this.outputWriter.flush();
        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    @Override
    public void displayMessage(final String message) {
        try {
            this.outputWriter.append(message + System.lineSeparator());
            this.outputWriter.flush();
        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private FrameworkRepresentation extractAndOrParseFramework(final String passedPath) {
        FrameworkRepresentation framework = null;
        final VirageParseJob parseJob;

        String path = passedPath;
        if (!path.endsWith(".pl")) {
            if (path.endsWith("ROOT")) {
                path = path.substring(0, path.length() - "ROOT".length());
            }

            final File file = new File(path);
            String newFileName = null;
            if (file.isDirectory()) {
                final File[] files = file.listFiles();

                final List<File> plFiles = new LinkedList<File>();
                for (final File child : files) {
                    if (child.getAbsolutePath().endsWith(".pl")) {
                        plFiles.add(child);
                    }
                }

                if (!plFiles.isEmpty()) {
                    final int idx = this.chooseAlternative("The following (E)PL files were found. "
                            + "Do you want to use one of those and skip (E)PL generation? "
                            + "This saves time if no changes to the Isabelle theories were "
                            + "made since the (E)PL file was created.", plFiles);

                    if (idx != -1) {
                        return this.extractAndOrParseFramework(plFiles.get(idx).getAbsolutePath());
                    } else {
                        while (true) {
                            newFileName = this
                                    .requestString("Please enter a name for the new file.");

                            if (!newFileName.endsWith(".pl")) {
                                newFileName += ".pl";

                                final File checkExistenceFile = new File(
                                        path + File.separator + newFileName);
                                if (checkExistenceFile.exists() && this.requestConfirmation(
                                        "File exists already. " + "Do you want to overwrite it?")) {
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            if (!ConfigReader.getInstance().hasIsabelle()) {
                this.displayMessage("Isabelle is not available. "
                        + "Please install Isabelle or supply an (E)PL-file directly.");

                return null;
            }

            final String sessionName;
            if (ConfigReader.getInstance().hasSessionName() && this.requestConfirmation(
                    "Configuration option " + "\"ISABELLE_SESSION_NAME\" is specified " + "as "
                            + this.addQuotationsToPath(ConfigReader.getInstance().getSessionName())
                            + ". Is this the name of the session specified within "
                            + "the given ROOT file?")) {

                sessionName = ConfigReader.getInstance().getSessionName();

                this.displayMessage("Extracting (E)PL file from session \"" + sessionName + "\" at "
                        + this.addQuotationsToPath(path) + ".\n" + "This might take some time.");
            } else {
                sessionName = this.requestString(
                        "Please input the name of " + "the session within this directory.");
            }

            final VirageExtractJob extractJob = new VirageExtractJob(this, path, sessionName,
                    newFileName);
            this.core.submit(extractJob);
            extractJob.waitFor();
            if (extractJob.getState().equals(VirageJobState.FAILED)) {
                return null;
            }
            framework = extractJob.getResult();

            parseJob = new VirageParseJob(this, new File(framework.getAbsolutePath()));
        } else {
            parseJob = new VirageParseJob(this, new File(path));
        }

        this.displayMessage("The following operation might take some time, "
                + "especially when ViRAGe is run with previously " + "un-built Isabelle sessions.");
        this.core.submit(parseJob);
        parseJob.waitFor();

        // Unelegant, but prevents race condition where user prompt is printed
        // before the result.
        try {
            Thread.sleep(100);
        } catch (final InterruptedException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        if (!parseJob.getState().equals(VirageJobState.FINISHED)) {
            return null;
        }

        return parseJob.getResult();
    }

    /**
     * Similar to run(), but creates its own new thread.
     */
    @Override
    public void launch() {
        this.thread = new Thread(this, "vcli");
        this.thread.start();
    }

    @Override
    public void notify(final VirageJob<?> job) {
        this.displayMessage(job.presentResult() + System.lineSeparator());
    }

    private void printBanner() {
        this.displayMessage(BANNER);
    }

    private void printPurpose() {
        this.displayMessage("# ViRAGe is a tool to generate voting rules and automatically ");
        this.displayMessage("# reason about their social choice properties.");
    }

    private void printSeparator() {
        this.displayMessage(SEPARATOR);
    }

    private void printSettings() {
        this.displayMessage("# Reading configuration from "
                + this.addQuotationsToPath(ConfigReader.getConfigPath()) + ".");
        this.displayMessage("#");
        this.displayMessage("# The following settings were found: ");

        final List<String> propertyNames = new LinkedList<String>();
        for (final String s : ConfigReader.getInstance().getAllProperties().keySet()) {
            propertyNames.add(s);
        }
        Collections.sort(propertyNames);

        String lastPrefix = "";
        for (final String s : propertyNames) {
            if (!lastPrefix.equals(s.split("_")[0])) {
                this.displayMessage("#");
                lastPrefix = s.split("_")[0];
            }

            String value = ConfigReader.getInstance().getAllProperties().get(s).replaceAll(";",
                    ";\n#\t");

            if (value.isEmpty()) {
                value = "NOT_SET";
            }

            this.displayMessage("# " + s.toUpperCase() + ":\n#\t" + value);
        }

        if (this.requestConfirmation(
                "# Do you want to edit these settings? " + "This will open the configuration "
                        + "file via \'xdg-open\' or your chosen text editor"
                        + " and require a restart of ViRAGe.")) {

            try {
                this.displayMessage("# If ViRAGe freezes or terminates without opening an editor, "
                        + "please try another one by setting \"SYSTEM_TEXT_EDITOR\" in "
                        + ConfigReader.getConfigPath() + ". At the moment, only windowed "
                        + "editors (mousepad, ...) are supported, not in-terminal ones "
                        + "(nano, vim, ...).");

                ConfigReader.getInstance().openConfigFileForEditing();

                this.displayMessage("# Please restart ViRAGe for the changes to take effect.");

                System.exit(0);
            } catch (final ExternalSoftwareUnavailableException e) {
                this.displayMessage(
                        "# No text editor available. Please set the configuration option "
                                + "\"SYSTEM_TEXT_EDITOR\" or the environment variable \"EDITOR\".");
            }
        }
    }

    private void printTimestamp() {
        final DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        final LocalDateTime now = LocalDateTime.now();
        this.displayMessage(
                "# Version " + VirageCore.getVersion() + ", Timestamp: " + dtf.format(now));
        this.displayMessage("# If you need help, type \"help\", \"h\" or \"?\".");
        this.displayMessage("# To exit ViRAGe, type \"exit\".");
    }

    private String requestCompositionString() {
        boolean invalid = false;

        String compositionString = this
                .requestString("Please input a composition (in Prolog format) "
                        + "or leave empty to display available components.");

        compositionString = StringUtils.removeWhitespace(compositionString);

        if (!compositionString.isEmpty()) {
            try {
                DecompositionTree.parseString(compositionString);
            } catch (final IllegalArgumentException e) {
                LOGGER.error("\"" + compositionString
                        + "\" could not be parsed. Please check the brackets and try again.");
                invalid = true;
            }
        }

        if (compositionString.isEmpty() || invalid) {
            final List<String> sortedStrings = new ArrayList<String>();
            for (final Component c : this.core.getFrameworkRepresentation().getComponents()) {
                sortedStrings.add("\t" + c.toString());
            }
            for (final ComposableModule c : this.core.getFrameworkRepresentation()
                    .getComposableModules()) {
                sortedStrings.add("\t" + c.toString());
            }
            for (final CompositionalStructure c : this.core.getFrameworkRepresentation()
                    .getCompositionalStructures()) {
                sortedStrings.add("\t" + c.toString());
            }
            Collections.sort(sortedStrings);
            for (final String s : sortedStrings) {
                this.displayMessage(s);
            }

            return this.requestCompositionString();
        }

        return compositionString;
    }

    /**
     * Requests confirmation from the user.
     *
     * @return true if user answers yes, false if user answers no
     */
    @Override
    public boolean requestConfirmation(final String message) {
        while (true) {
            final String input = this.requestString(message + " (y/n)");

            if (input.startsWith("y")) {
                return true;
            } else if (input.startsWith("n")) {
                return false;
            }

            this.displayMessage("Please try again.");
        }
    }

    // TODO Change to return List<Property> maybe?
    private String requestPropertyString() {
        final FrameworkRepresentation framework = this.core.getFrameworkRepresentation();

        String propertiesString = this.requestString("Please input the desired "
                + "properties (separated by ',') or leave empty to display available properties.");

        propertiesString = StringUtils.removeWhitespace(propertiesString);

        boolean invalid = false;

        if (!propertiesString.isEmpty()) {
            final String[] propertyStrings = propertiesString.split(",");
            for (final String propertyString : propertyStrings) {
                final Property property = framework.getProperty(propertyString);

                if (property == null) {
                    LOGGER.error("Property \"" + propertyString + "\" is undefined.");

                    invalid = true;
                    break;
                }
            }
        }

        if (propertiesString.isEmpty() || invalid) {
            final List<String> sortedProps = new ArrayList<String>();
            for (final Property p : this.core.getFrameworkRepresentation().getProperties()) {
                if (p.getArity() == 1) {
                    sortedProps.add("\t" + p.getName());
                }
            }
            Collections.sort(sortedProps);

            for (final String s : sortedProps) {
                this.displayMessage(s);
            }

            return this.requestPropertyString();
        }

        return propertiesString;
    }

    @Override
    public String requestString(final String message) {
        while (true) {
            this.displayMessage(message);

            this.displayInputMarker();
            final String input = this.scanner.nextLine();

            switch (input) {
            case "?":
            case "h":
            case "help":
                this.displayHelp();
                break;
            case "exit":
                this.core.submit(new VirageExitJob(this, 0));
                break;
            default:
                return input;
            }
        }
    }

    @Override
    public void run() {
        LOGGER.info("Started VirageCommandLineInterface.");

        final String defaultPath = "./src/test/resources/framework.pl";

        String path;

        boolean firstTry = true;

        while (true) {
            if (ConfigReader.getInstance().hasPathToRootFile() && firstTry
                    && this.requestConfirmation(
                            "Configuration option \"ISABELLE_PATH_TO_ROOT_FILE\" "
                                    + "is specified as "
                                    + this.addQuotationsToPath(
                                            ConfigReader.getInstance().getPathToRootFile())
                                    + ". " + "Do you want to use this Isabelle ROOT file to "
                                    + "generate an (E)PL file?")) {
                path = ConfigReader.getInstance().getPathToRootFile();

                firstTry = false;
            } else {
                path = this.requestString("Please input the path to an (E)PL file or "
                        + "an Isabelle ROOT file containing exactly one session specification. "
                        + "(Press ENTER for default: " + this.addQuotationsToPath(defaultPath)
                        + ")");
            }

            if (path.isEmpty()) {
                path = defaultPath;
            }

            if (this.extractAndOrParseFramework(path) != null) {
                break;
            }
        }

        while (true) {
            final String arg = this
                    .requestString("Do you want to (g)enerate a composition, (a)nalyze one, "
                            + "(p)rove a claim,\n"
                            + "generate (I)sabelle proofs, generate (S)cala code "
                            + "or generate (C) code?");

            VirageJob<?> job = null;

            // TODO Refactor to enum
            if (arg.equals("g")) {
                job = this.createGenerationQuery();
            } else if (arg.equals("a")) {
                job = this.createAnalysisQuery();
            } else if (arg.equals("p")) {
                job = this.createProofQuery();
            } else if (arg.equals("I")) {
                job = this.createIsabelleQuery();
            } else if (arg.equals("S")) {
                job = this.createCodeGenerationQuery();
            } else if (arg.equals("C")) {
                job = this.createCCodeGenerationQuery();
            } else {
                this.displayMessage("Please try again.");
                continue;
            }

            this.core.submit(job);
            // VirageCore is intended to work on jobs asynchronously
            // and that is perfectly possible. It just does not make
            // too much sense when using a CLI, so it is disabled.
            job.waitFor();
        }
    }
}
