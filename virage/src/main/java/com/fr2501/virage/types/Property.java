package com.fr2501.virage.types;

import java.util.List;

import com.fr2501.util.StringUtils;

/**
 * Represents a property defined in the modular framework.
 *
 */
public final class Property implements Parameterized {
    private final String name;
    private final int arity;
    private final List<ComponentType> parameters;

    /**
     * Simple constructor.
     *
     * @param name the name
     * @param parameters the parameters
     */
    public Property(final String name, final List<ComponentType> parameters) {
        this.name = name;
        this.arity = parameters.size();
        this.parameters = parameters;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (this.getClass() != obj.getClass()) {
            return false;
        }
        final Property other = (Property) obj;
        if (this.arity != other.arity) {
            return false;
        }
        if (this.name == null) {
            if (other.name != null) {
                return false;
            }
        } else if (!this.name.equals(other.name)) {
            return false;
        }
        return true;
    }

    public int getArity() {
        return this.arity;
    }

    /**
     * Instantiates a property with values in the given order.
     *
     * @param strings the parameters
     * @return the instantiated string
     */
    public String getInstantiatedString(final List<String> strings) {
        if (strings.size() != this.parameters.size()) {
            throw new IllegalArgumentException();
        }

        final String res = this.name + "(" + StringUtils.printCollection(strings) + ")";

        return res;
    }

    /**
     * Instantiates a unary property with the given string.
     *
     * @param string the instantiation string
     * @return the instantiated string
     */
    public String getInstantiatedString(final String string) {
        if (this.parameters.size() != 1) {
            throw new IllegalArgumentException();
        }

        final String res = this.name + "(" + string + ")";

        return res;
    }

    /**
     * Instantiates a property with values in the given order, leaving out the property's name.
     *
     * @param strings the parameters
     * @return the instantiated string
     */
    public String getInstantiatedStringWithoutName(final List<String> strings) {
        if (strings.size() != this.parameters.size()) {
            throw new IllegalArgumentException();
        }

        final String res = "(" + StringUtils.printCollection(strings) + ")";

        return res;
    }

    /**
     * Instantiates a unary property with the given string, leaving out the property's name.
     *
     * @param string the instantiation string
     * @return the instantiated string
     */
    public String getInstantiatedStringWithoutName(final String string) {
        if (this.parameters.size() != 1) {
            throw new IllegalArgumentException();
        }

        final String res = "(" + string + ")";

        return res;
    }

    public String getName() {
        return this.name;
    }

    @Override
    public List<ComponentType> getParameters() {
        return this.parameters;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + this.arity;
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        return result;
    }

    @Override
    public String toString() {
        final String res = this.name + "(" + StringUtils.printCollection(this.parameters) + ")";

        return res;
    }
}
