package com.fr2501.virage.prolog;

import java.util.LinkedList;
import java.util.List;

/**
 * 
 * A simple data object to contain a single Prolog predicate and its parameters
 *
 */
public class PrologPredicate {
  private String name;
  private List<PrologPredicate> parameters;
  private int arity;
  private int depth;

  public PrologPredicate(String name, List<PrologPredicate> parameters) {
    this.name = name;
    this.parameters = parameters;
    this.arity = parameters.size();

    this.depth = 0;
    for (PrologPredicate parameter : this.parameters) {
      if (parameter.depth >= this.depth) {
        this.depth = parameter.depth + 1;
      }
    }
  }

  /**
   * Creates a predicate without any parameters (arity 0).
   * 
   * @param name the name of the predicate
   */
  public PrologPredicate(String name) {
    this.name = name;
    this.parameters = new LinkedList<PrologPredicate>();
    this.arity = 0;
  }

  public String getName() {
    return this.name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public List<PrologPredicate> getParameters() {
    return this.parameters;
  }

  public int getArity() {
    return this.arity;
  }

  public int getDepth() {
    return this.depth;
  }

  /**
   * Checks whether a PrologPredicate is a variable
   * 
   * @return true if this is a variable, false otherwise
   */
  public boolean isVariable() {
    return this.name.matches("[A-Z_][a-zA-Z_0-9]*");
  }

  @Override
  public String toString() {
    String res = "";

    res += this.name;
    if (this.arity > 0) {
      res += "(";

      for (int i = 0; i < this.arity; i++) {
        res += this.parameters.get(i).toString();
        if (i < this.arity - 1) {
          res += ",";
        }
      }

      res += ")";
    }

    return res;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + arity;
    result = prime * result + ((name == null) ? 0 : name.hashCode());
    result = prime * result + ((parameters == null) ? 0 : parameters.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    PrologPredicate other = (PrologPredicate) obj;
    if (arity != other.arity)
      return false;
    if (name == null) {
      if (other.name != null)
        return false;
    } else if (!name.equals(other.name))
      return false;
    if (parameters == null) {
      if (other.parameters != null)
        return false;
    }

    if (this.parameters.size() != other.parameters.size()) {
      return false;
    }

    for (int i = 0; i < this.parameters.size(); i++) {
      if (!this.parameters.get(i).equals(other.parameters.get(i))) {
        return false;
      }
    }

    return true;
  }
}
