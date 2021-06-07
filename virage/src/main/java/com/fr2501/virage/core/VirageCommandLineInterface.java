package com.fr2501.virage.core;

import com.fr2501.util.StringUtils;
import com.fr2501.virage.jobs.VirageAnalyzeJob;
import com.fr2501.virage.jobs.VirageDummyJob;
import com.fr2501.virage.jobs.VirageExitJob;
import com.fr2501.virage.jobs.VirageExtractJob;
import com.fr2501.virage.jobs.VirageGenerateJob;
import com.fr2501.virage.jobs.VirageIsabelleGenerateJob;
import com.fr2501.virage.jobs.VirageIsabelleGenerateScalaJob;
import com.fr2501.virage.jobs.VirageIsabelleVerifyJob;
import com.fr2501.virage.jobs.VirageJob;
import com.fr2501.virage.jobs.VirageJobState;
import com.fr2501.virage.jobs.VirageParseJob;
import com.fr2501.virage.jobs.VirageProveJob;
import com.fr2501.virage.types.CompositionProof;
import com.fr2501.virage.types.DecompositionTree;
import com.fr2501.virage.types.FrameworkRepresentation;
import com.fr2501.virage.types.Property;
import java.io.File;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * 
 * A simple command line interface for ViRAGe.
 *
 */
public class VirageCommandLineInterface implements VirageUserInterface {
  private final Logger logger = LogManager.getLogger(VirageCommandLineInterface.class);
  private Scanner scanner;
  private VirageCore core;

  private CommandLineProgressIndicator clpi;

  private static final String SEPARATOR 
      = "###########################################################";
  private static final String BANNER = "#\n"
      + "# Y88b      / ,e, 888~-_        e       e88~~\\           \n"
      + "#  Y88b    /   \"  888   \\      d8b     d888      e88~~8e \n"
      + "#   Y88b  /   888 888    |    /Y88b    8888 __  d888  88b\n"
      + "#    Y888/    888 888   /    /  Y88b   8888   | 8888__888\n"
      + "#     Y8/     888 888_-~    /____Y88b  Y888   | Y888    ,\n"
      + "#      Y      888 888 ~-_  /      Y88b  \"88__/   \"88___/ \n#";

  private Thread thread;

  protected VirageCommandLineInterface(VirageCore core) {
    logger.info("Initialising VirageCommandLineInterface.");

    this.printSeparator();
    System.out.println(BANNER);
    this.printSeparator();

    DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    LocalDateTime now = LocalDateTime.now();
    System.out.println("# Version " + core.getVersion() + ", Timestamp: " + dtf.format(now));
    System.out.println("# Using " + ConfigReader.getInstance().getConfigPath() + ".");

    this.printSeparator();

    ConfigReader.getInstance().checkAvailabilityAndPrintVersions();

    this.printSeparator();

    this.scanner = new Scanner(System.in);
    this.core = core;

    this.clpi = new CommandLineProgressIndicator();
  }

  private void printSeparator() {
    System.out.println(SEPARATOR);
  }

  /**
   * Similar to run(), but creates its own new thread.
   */
  public void launch() {
    this.thread = new Thread(this, "vcli");
    this.thread.start();
  }

  @Override
  public void run() {
    logger.info("Started VirageCommandLineInterface.");

    String defaultPath = "./src/test/resources/framework.pl";

    String path;

    boolean firstTry = true;

    while (true) {
      System.out.println("Please input the path to an EPL file or an Isabelle root directory. "
          + "(default: " + defaultPath + ")");
      if (ConfigReader.getInstance().hasPathToRootFile() && firstTry) {
        System.out.println(
            "Configuration option \"path_to_root_file\" " + "is specified and will be used.");

        path = ConfigReader.getInstance().getPathToRootFile();

        firstTry = false;
      } else {
        path = this.scanner.nextLine();
      }

      if (path.equals("")) {
        path = defaultPath;
      }

      if (this.extractAndOrParseFramework(path) != null) {
        break;
      }
    }

    while (true) {
      System.out.println("Do you want to (g)enerate a composition, (a)nalyze one, "
          + "(p)rove a claim,\n" + "generate (I)sabelle code or generate (S)cala code?");
      String arg = this.scanner.nextLine();

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
      } else if (arg.equals("exit")) {
        job = new VirageExitJob(this, 0);
        this.core.submit(job);
        return;
      } else {
        System.out.println("Please try again.");
        continue;
      }

      this.core.submit(job);
      // VirageCore is intended to work on jobs asynchronously
      // and that is perfectly possible. It just does not make
      // too much sense when using a CLI, so it is disabled.
      job.waitFor();
    }
  }

  private FrameworkRepresentation extractAndOrParseFramework(String path) {
    FrameworkRepresentation framework = null;
    VirageParseJob parseJob;

    if (!path.endsWith(".pl")) {
      File file = new File(path);
      if (file.isDirectory()) {
        File[] files = file.listFiles();

        outer: for (File child : files) {
          if (child.getAbsolutePath().endsWith(".pl")) {
            while (true) {
              boolean conf = this.requestConfirmation(
                  "EPL file " + child.getAbsolutePath() + " found. " + "Do you want to use it?");

              if (conf) {
                return this.extractAndOrParseFramework(child.getAbsolutePath());
              } else {
                continue outer;
              }
            }
          }
        }
      }

      if (!ConfigReader.getInstance().hasIsabelle()) {
        System.out
            .println("Isabelle is not available. Please install or supply an EPL-file directly.");

        return null;
      }

      String sessionName;
      System.out.println("Please input the name of the session within this directory.");
      if (ConfigReader.getInstance().hasSessionName()) {
        System.out.println("Configuration option \"session_name\" is specified and will be used.");

        sessionName = ConfigReader.getInstance().getSessionName();

        System.out.println("Extracting framework from session \"" + sessionName + "\" at " + path
            + ".\n" + "This might take some time.");
      } else {
        sessionName = this.scanner.nextLine();
      }

      VirageExtractJob extractJob = new VirageExtractJob(this, path, sessionName);
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

    this.core.submit(parseJob);
    parseJob.waitFor();

    if (!parseJob.getState().equals(VirageJobState.FINISHED)) {
      return null;
    }

    return parseJob.getResult();
  }

  @Override
  public void notify(VirageJob<?> job) {
    System.out.println(job.toString());
  }

  private VirageGenerateJob createGenerationQuery() {
    String propertyString = this.requestPropertyString();

    List<String> properties = StringUtils.separate(",", propertyString);

    VirageGenerateJob res = new VirageGenerateJob(this, properties);
    return res;
  }

  private VirageAnalyzeJob createAnalysisQuery() {
    String composition = this.requestCompositionString();

    String propertyString = this.requestPropertyString();

    List<String> properties = StringUtils.separate(",", propertyString);

    VirageAnalyzeJob res = new VirageAnalyzeJob(this, composition, properties);
    return res;
  }

  private VirageProveJob createProofQuery() {
    String composition = this.requestCompositionString();

    String propertyString = this.requestPropertyString();

    return this.createProofQuery(composition, propertyString);
  }

  private VirageProveJob createProofQuery(String composition, String propertyString) {
    List<String> properties = StringUtils.separate(",", propertyString);

    VirageProveJob res = new VirageProveJob(this, composition, properties);
    return res;
  }

  private VirageJob<?> createIsabelleQuery() {
    String composition = this.requestCompositionString();

    String propertyString = this.requestPropertyString();

    String defaultPath = "./target/generated-sources/";
    System.out.println(
        "Please specify a directory for the generated theory file. (default: " + defaultPath + ")");
    String outputPath = this.scanner.nextLine();
    if (outputPath.equals("")) {
      outputPath = defaultPath;
    }

    boolean verify = true;
    while (true) {
      System.out.println("Shall the resulting theory file be verified automatially? [(y)es/(n)o]");
      String verifyString = this.scanner.nextLine();

      if (verifyString.equals("y")) {
        break;
      } else if (verifyString.equals("n")) {
        verify = false;
        break;
      }
    }

    VirageProveJob proveJob = this.createProofQuery(composition, propertyString);
    this.core.submit(proveJob);
    proveJob.waitFor();

    if (proveJob.getState() == VirageJobState.FAILED) {
      logger.warn("Proving the given claims failed.");
      return new VirageDummyJob(this);
    }

    List<List<CompositionProof>> proofLists = proveJob.getResult();
    List<CompositionProof> bestProof = new LinkedList<CompositionProof>();
    for (List<CompositionProof> proof : proofLists) {
      if (proof.size() > bestProof.size()) {
        bestProof = proof;
      }
    }

    VirageIsabelleGenerateJob generateJob = new VirageIsabelleGenerateJob(this, composition,
        bestProof, outputPath);
    if (!verify) {
      return generateJob;
    }
    this.core.submit(generateJob);
    generateJob.waitFor();

    VirageIsabelleVerifyJob verifyJob = new VirageIsabelleVerifyJob(this, generateJob.getResult());
    return verifyJob;
  }

  private VirageIsabelleGenerateScalaJob createCodeGenerationQuery() {
    String composition = this.requestCompositionString();

    VirageIsabelleGenerateScalaJob res = new VirageIsabelleGenerateScalaJob(this, composition);
    return res;
  }

  // TODO Change to return List<Property> maybe?
  private String requestPropertyString() {
    FrameworkRepresentation framework = this.core.getFrameworkRepresentation();

    System.out.println(
        "Please input the desired properties (separated by ',') "
        + "or leave empty to display available properties.");
    String propertiesString = this.scanner.nextLine();

    boolean invalid = false;

    if (!propertiesString.isEmpty()) {
      String[] propertyStrings = propertiesString.split(",");
      for (String propertyString : propertyStrings) {
        Property property = framework.getProperty(propertyString);

        if (property == null) {
          logger.error("Property \"" + propertyString + "\" is undefined.");

          invalid = true;
          break;
        }
      }
    }

    if (propertiesString.isEmpty() || invalid) {
      System.out.println(
          "Available properties: " + StringUtils.printCollection(framework.getProperties()));

      return this.requestPropertyString();
    }

    return propertiesString;
  }

  private String requestCompositionString() {
    FrameworkRepresentation framework = this.core.getFrameworkRepresentation();
    boolean invalid = false;

    System.out.println(
        "Please input a composition (in Prolog format) "
        + "or leave empty to display available components.");
    String compositionString = this.scanner.nextLine();

    if (!compositionString.isEmpty()) {
      try {
        DecompositionTree.parseString(compositionString);
      } catch (Exception e) {
        logger.error("\"" + compositionString
            + "\" could not be parsed. Please check the brackets and try again.");
        invalid = true;
      }
    }

    if (compositionString.isEmpty() || invalid) {
      System.out
          .println("Available components: " + StringUtils.printCollection(framework.getComponents())
              + "," + StringUtils.printCollection(framework.getCompositionalStructures()));

      return this.requestCompositionString();
    }

    return compositionString;
  }

  public boolean requestConfirmation(String message) {
    this.clpi.hide();

    boolean returnValue;

    loop: while (true) {
      System.out.println(message + " (y/n)");

      String input = this.scanner.nextLine();

      switch (input) {
      case "y":
        returnValue = true;
        break loop;
      case "n":
        returnValue = false;
        break loop;
      }
    }

    this.clpi.show();
    return returnValue;
  }

  public ProgressIndicator spawnProgressIndicator() {
    return this.clpi;
  }
}
