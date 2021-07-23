package com.fr2501.virage.jobs;

import java.io.File;
import java.io.IOException;

import com.fr2501.virage.core.VirageUserInterface;
import com.fr2501.virage.prolog.MalformedEplFileException;
import com.fr2501.virage.types.FrameworkRepresentation;

/**
 * A {@link VirageJob} used to parse an (E)PL file and pass it to its executing core.
 *
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
     * @param file the file
     */
    public VirageParseJob(final VirageUserInterface issuer, final File file) {
        super(issuer);

        this.file = file;
    }

    @Override
    public void concreteExecute() throws IOException, MalformedEplFileException {

        this.result = this.executingCore.getExtendedPrologParser().parseFramework(this.file, true);
        this.executingCore.setFrameworkRepresentation(this.result);

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
