package edu.kit.kastel.formal.virage.jobs;

import java.io.File;
import java.io.IOException;

import edu.kit.kastel.formal.virage.core.VirageUserInterface;
import edu.kit.kastel.formal.virage.prolog.MalformedEplFileException;
import edu.kit.kastel.formal.virage.types.FrameworkRepresentation;

/**
 * A {@link VirageJob} used to parse an (E)PL file and pass it to its executing core.
 *
 * @author VeriVote
 */
public final class VirageParseJob extends VirageJobWithExplicitResult<FrameworkRepresentation> {
    /**
     * The (E)PL file to be parsed.
     */
    private final File file;

    /**
     * Simple constructor.
     *
     * @param issuer the issuing ui
     * @param fileValue the file
     */
    public VirageParseJob(final VirageUserInterface issuer, final File fileValue) {
        super(issuer);

        this.file = fileValue;
    }

    @Override
    public void concreteExecute() throws IOException, MalformedEplFileException {

        this.setResult(this.getExecutingCore()
                .getExtendedPrologParser().parseFramework(this.file, true));
        this.getExecutingCore().setFrameworkRepresentation(this.getResult());

    }

    @Override
    public boolean externalSoftwareAvailable() {
        return true;
    }

    @Override
    public String getDescription() {
        return "Parsing (E)PL file and invoking the Isabelle session it references ...";

    }

    @Override
    public String presentConcreteResult() {
        final String res = "Successfully loaded (E)PL file at \'" + this.file.getAbsolutePath()
            + "\'.\n";

        return res;
    }
}
