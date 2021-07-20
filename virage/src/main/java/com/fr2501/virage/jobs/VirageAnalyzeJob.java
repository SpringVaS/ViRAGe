package com.fr2501.virage.jobs;

import java.util.LinkedList;
import java.util.List;

import com.fr2501.util.StringUtils;
import com.fr2501.virage.core.ConfigReader;
import com.fr2501.virage.core.VirageSearchManager;
import com.fr2501.virage.core.VirageUserInterface;
import com.fr2501.virage.types.BooleanWithUncertainty;
import com.fr2501.virage.types.DecompositionTree;
import com.fr2501.virage.types.FrameworkRepresentation;
import com.fr2501.virage.types.Property;
import com.fr2501.virage.types.SearchResult;

/**
 * A {@link VirageJob} used to analyze a composition.
 *
 */
public final class VirageAnalyzeJob
extends VirageJobWithExplicitResult<List<List<SearchResult<BooleanWithUncertainty>>>> {
    private final List<String> propertyStrings;
    private List<Property> properties;
    private final DecompositionTree tree;

    private FrameworkRepresentation framework;
    private VirageSearchManager manager;

    /**
     * Simple constructor.
     *
     * @param issuer the issuer
     * @param tree the tree
     * @param properties the properties
     */
    public VirageAnalyzeJob(final VirageUserInterface issuer, final String tree,
            final List<String> properties) {
        super(issuer);

        this.tree = DecompositionTree.parseString(tree);
        this.propertyStrings = properties;
    }

    @Override
    public void concreteExecute() {
        this.framework = this.executingCore.getFrameworkRepresentation();
        this.manager = this.executingCore.getSearchManager();

        this.properties = new LinkedList<Property>();

        for (final String s : this.propertyStrings) {
            this.properties.add(this.framework.getProperty(s));
        }

        this.result = this.manager.analyzeComposition(this.tree, this.properties);
    }

    @Override
    public boolean externalSoftwareAvailable() {
        return (ConfigReader.getInstance().hasJpl());
    }

    @Override
    public String getDescription() {
        return "Analyzing a composition ...";
    }

    @Override
    public String presentConcreteResult() {
        String prop = "properties";
        if (this.properties.size() == 1) {
            prop = "property";
        }

        boolean hasProperties = false;
        for (final List<SearchResult<BooleanWithUncertainty>> resultList : this.result) {
            for (final SearchResult<BooleanWithUncertainty> result : resultList) {
                if (result.hasValue() && result.getValue() == BooleanWithUncertainty.TRUE) {
                    hasProperties = true;
                    break;
                }
            }
        }

        if (hasProperties) {
            return this.tree.toString() + " has the " + prop + " "
                    + StringUtils.printCollection(this.properties) + "";
        } else {
            return this.tree.toString() + " cannot be shown to have the " + prop + " "
                    + StringUtils.printCollection(this.properties) + "";
        }
    }
}
