package com.fr2501.virage.jobs;

import com.fr2501.virage.core.VirageUserInterface;
import com.fr2501.virage.isabelle.IsabelleTheoryGenerator;

public class VirageIsabelleJob extends VirageExecutorJob<IsabelleTheoryGenerator, Void> {

	public VirageIsabelleJob(VirageUserInterface issuer) {
		super(issuer);
	}

	@Override
	protected void concreteExecute() throws Exception {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Void getResult() {
		throw new UnsupportedOperationException();
	}

}
