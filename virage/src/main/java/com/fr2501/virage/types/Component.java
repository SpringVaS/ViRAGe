package com.fr2501.virage.types;

import java.util.LinkedList;
import java.util.List;

/**
 * 
 * A component of the modular framework (e.g. composable modules, aggregators ...)
 *
 */
public class Component implements TypedAndParameterized {
	private ComponentType type;
	private String name;
	private List<ComponentType> parameters;
	
	public Component(ComponentType type, String name, List<ComponentType> parameters) {
		this.type = type;
		this.name = name;
		this.parameters = parameters;
	}
	
	public Component(ComponentType type, String name) {
		this(type, name, new LinkedList<ComponentType>());
	}
	
	@Override
	public ComponentType getType() {
		return this.type;
	}

	public String getName() {
		return this.name;
	}
	
	@Override
	public List<ComponentType> getParameters() {
		return this.parameters;
	}
}
