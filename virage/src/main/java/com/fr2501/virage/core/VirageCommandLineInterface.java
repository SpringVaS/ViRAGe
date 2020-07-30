package com.fr2501.virage.core;

import java.io.File;
import java.util.List;
import java.util.Scanner;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fr2501.util.StringUtils;
import com.fr2501.virage.jobs.VirageAnalyzeJob;
import com.fr2501.virage.jobs.VirageExitJob;
import com.fr2501.virage.jobs.VirageGenerateJob;
import com.fr2501.virage.jobs.VirageIsabelleJob;
import com.fr2501.virage.jobs.VirageJob;
import com.fr2501.virage.jobs.VirageJobState;
import com.fr2501.virage.jobs.VirageParseJob;
import com.fr2501.virage.jobs.VirageProveJob;

/**
 * 
 * A simple command line interface for ViRAGe
 *
 */
public class VirageCommandLineInterface implements VirageUserInterface {
	private static final Logger logger = LogManager.getLogger(VirageCommandLineInterface.class);
	private Scanner scanner;
	private VirageCore core;
	
	private Thread thread;
	
	protected VirageCommandLineInterface(VirageCore core) {
		logger.info("Initialising VirageCommandLineInterface.");
		
		this.scanner = new Scanner(System.in);
		this.core = core;
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
		
		System.out.println("Please input the absolute path to an EPL file.");
		String path = this.scanner.nextLine();
		
		VirageParseJob parseJob = new VirageParseJob(this, new File(path));
		this.core.submit(parseJob);
		while(parseJob.getState() != VirageJobState.FINISHED) {
			if(parseJob.getState() == VirageJobState.FAILED) {
				System.out.println("Please input the absolute path to an EPL file.");
				path = this.scanner.nextLine();
				parseJob = new VirageParseJob(this, new File(path));
				this.core.submit(parseJob);
			}
		}
		
		while(true) {
			System.out.println("Do you want to (g)enerate a composition, (a)nalyze one, (p)rove a claim"
					+ " or generate (I)sabelle code?");
			String arg = this.scanner.nextLine();
			
			VirageJob<?> job = null;
			
			if(arg.equals("g")) {
				job = this.createGenerationQuery();
			} else if(arg.equals("a")) {
				job = this.createAnalysisQuery();
			} else if(arg.equals("p")) {
				job = this.createProofQuery();
			} else if(arg.equals("I")) {
				job = this.createIsabelleQuery();
			} else if(arg.equals("exit")) {
				job = new VirageExitJob(this, 0);
				this.core.submit(job);
				return;
			} else {
				System.out.println("Please try again.");
				continue;
			}
			
			this.core.submit(job);
		}
	}
	
	@Override
	public void notify(VirageJob<?> job) {
		System.out.println(job.toString());
	}
	
	private VirageGenerateJob createGenerationQuery() {
		System.out.println("Please input the desired properties (separated by ',').");
		String propertyString = this.scanner.nextLine();

		List<String> properties = StringUtils.separate(",", propertyString);
		
		VirageGenerateJob res = new VirageGenerateJob(this, properties);
		this.core.submit(res);
		return res;
	}
	
	private VirageAnalyzeJob createAnalysisQuery() {
		System.out.println("Please input a composition (in Prolog format).");
		String composition = this.scanner.nextLine();
		
		System.out.println("Please input the desired properties (separated by ',').");
		String propertyString = this.scanner.nextLine();

		List<String> properties = StringUtils.separate(",", propertyString);
		
		VirageAnalyzeJob res = new VirageAnalyzeJob(this, composition, properties);
		this.core.submit(res);
		return res;
	}
	
	private VirageProveJob createProofQuery() {
		System.out.println("Please input a composition (in Prolog format).");
		String composition = this.scanner.nextLine();
		
		System.out.println("Please input the desired properties (separated by ',').");
		String propertyString = this.scanner.nextLine();

		List<String> properties = StringUtils.separate(",", propertyString);
		
		VirageProveJob res = new VirageProveJob(this, composition, properties);
		this.core.submit(res);
		return res;
	}
	
	private VirageIsabelleJob createIsabelleQuery() {
		System.out.println("Please input the absoulte path to an Isabelle theory folder.");
		String theoryPathString = this.scanner.nextLine();
		
		System.out.println("Please input a composition (in Prolog format).");
		String composition = this.scanner.nextLine();
		
		System.out.println("Please input the desired properties (separated by ',').");
		String propertyString = this.scanner.nextLine();

		List<String> properties = StringUtils.separate(",", propertyString);
		
		return null;
	}
}
