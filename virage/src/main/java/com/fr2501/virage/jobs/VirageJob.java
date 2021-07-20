package com.fr2501.virage.jobs;

import java.time.Instant;

import com.fr2501.util.SystemUtils;
import com.fr2501.virage.core.VirageCore;
import com.fr2501.virage.core.VirageUserInterface;

/**
 * Wrapper class for all tasks to be completed by {@link com.fr2501.virage.core.VirageCore}. Require
 * a {@link VirageUserInterface} as listener.
 *
 * @param <T> the result type
 */
public abstract class VirageJob<T> {
    /**
     * The next available ID for a new job.
     */
    private static long NEXT_ID;

    /**
     * The core object executing this job.
     */
    protected VirageCore executingCore;
    /**
     * The current state of this job.
     */
    protected VirageJobState state;

    /**
     * A message containing the cause of an error, if one occurred.
     */
    protected String errorMessage = "";

    /**
     * The issuing ui.
     */
    private final VirageUserInterface issuer;

    /**
     * The ID of this job.
     */
    private final long id;

    /**
     * Time when this job was accepted by a core.
     */
    private final long timeIssued;
    /**
     * Starting time of this job.
     */
    private long timeStarted;
    /**
     * Finishing time of this job.
     */
    private long timeFinished;

    /**
     * Simple constructor.
     *
     * @param issuer the issuing ui
     */
    public VirageJob(final VirageUserInterface issuer) {
        this.issuer = issuer;
        this.id = VirageJob.NEXT_ID;
        VirageJob.NEXT_ID++;

        this.timeIssued = System.currentTimeMillis();

        this.state = VirageJobState.PENDING;
    }

    /**
     * Runs the job and notifies its issuer on termination. Should only be ran after checking
     * isReadyToExecute(), otherwise behaviour is undefined.
     *
     * @param core the executing core
     */
    public void execute(final VirageCore core) {
        this.executingCore = core;
        this.setState(VirageJobState.RUNNING);

        try {
            this.concreteExecute();
            this.setState(VirageJobState.FINISHED);
            // this.concreteExecute() can throw virtually any runtime exception.
        } catch (final Exception e) {
            this.setState(VirageJobState.FAILED);
        }
    }

    /**
     * Checks whether all required external dependencies are satisfied.
     * @return true if they are, false otherwise
     */
    public abstract boolean externalSoftwareAvailable();

    /**
     * Returns a description of the job's task.
     * @return the description
     */
    public abstract String getDescription();

    /**
     * Returns the result of this job, if one is available.
     * @return the result
     */
    public abstract T getResult();

    public final synchronized VirageJobState getState() {
        return this.state;
    }

    /**
     * Pretty-printed result of this job.
     * @return a pretty-printed String
     */
    public abstract String presentConcreteResult();

    /**
     * Pretty-print job, safe to override.
     * @return a pretty-printed String representation of this job
     */
    public String presentResult() {
        String res = "";

        final float timeInMs = this.timeFinished - this.timeStarted;
        final float timeInS = timeInMs / 1000;

        res += "Started at " + SystemUtils.getTime() + ".\n";
        res += "Job ran for " + String.format("%.2f", timeInS) + " seconds.\n";

        if (this.state == VirageJobState.FINISHED) {
            res += this.presentConcreteResult() + "\n";
        } else {
            res += "Something went wrong while executing this job.\n";
            res += this.errorMessage;
        }

        res += "----------";

        return res;
    }

    /**
     * Sets the current state, also updates the timestamps if applicable.
     *
     * @param state the new state
     */
    public void setState(final VirageJobState state) {
        this.state = state;

        if (state == VirageJobState.RUNNING) {
            this.timeStarted = System.currentTimeMillis();
        } else if (state == VirageJobState.FAILED || state == VirageJobState.FINISHED) {
            this.timeFinished = System.currentTimeMillis();

            this.issuer.notify(this);
        }
    }

    /**
     * Safe to override.
     */
    @Override
    public String toString() {
        String res = "----------- " + this.getClass().getCanonicalName() + "\n";
        res += "ID: " + this.id + "\n";

        res += "Issued: " + Instant.ofEpochMilli(this.timeIssued).toString() + "\n";
        res += "Started: " + Instant.ofEpochMilli(this.timeStarted).toString() + "\n";
        res += "Finished: " + Instant.ofEpochMilli(this.timeFinished).toString() + "\n";
        res += "Time elapsed: " + (this.timeFinished - this.timeStarted) + " milliseconds \n";
        res += "-----\n";
        res += "State: " + this.state + "\n";

        return res;
    }

    /**
     * Halts execution until this is finished ({@link VirageJobState#FINISHED} or
     * {@link VirageJobState#FAILED}).
     */
    public void waitFor() {
        while (true) {
            boolean finished = false;
            synchronized (this) {
                finished = (this.getState() != VirageJobState.PENDING
                        && this.getState() != VirageJobState.RUNNING);
            }

            if (finished) {
                return;
            }

            try {
                Thread.sleep(100);
            } catch (final InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    /**
     * The actual implementation of the job's functionality.
     *
     * @throws Exception which will be caught by the {@link com.fr2501.virage.core.VirageCore}
     * object
     */
    protected abstract void concreteExecute() throws Exception;
}
