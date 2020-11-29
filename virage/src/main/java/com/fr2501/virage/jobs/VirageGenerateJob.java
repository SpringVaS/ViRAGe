package com.fr2501.virage.jobs;

import java.util.LinkedList;
import java.util.List;

import com.fr2501.virage.core.VirageSearchManager;
import com.fr2501.virage.core.VirageUserInterface;
import com.fr2501.virage.types.DecompositionTree;
import com.fr2501.virage.types.FrameworkRepresentation;
import com.fr2501.virage.types.Property;
import com.fr2501.virage.types.SearchResult;

/**
 * 
 * A {@link VirageJob} used for generating compositions
 *
 */
public class VirageGenerateJob extends VirageJobWithExplicitResult<List<SearchResult<DecompositionTree>>> {
	private List<String> propertyStrings;
	private List<Property> properties;
	
	private FrameworkRepresentation framework;
	private VirageSearchManager manager;
	
	public VirageGenerateJob(VirageUserInterface issuer, List<String> properties) {
		super(issuer);
		
		this.propertyStrings = properties;
	}

	@Override
	public void concreteExecute() {
		this.framework = this.executingCore.getFrameworkRepresentation();
		this.manager = this.executingCore.getSearchManager();
		
		this.properties = new LinkedList<Property>();
		
		for(String s: this.propertyStrings) {
			this.properties.add(this.framework.getProperty(s));
		}
		
		this.result = this.manager.generateComposition(properties);
	}

	@Override
	public List<SearchResult<DecompositionTree>> getResult() {
		return this.result;
	}
}