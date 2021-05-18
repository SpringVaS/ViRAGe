package com.fr2501.virage.jobs;

import com.fr2501.virage.core.ConfigReader;
import com.fr2501.virage.core.VirageUserInterface;
import com.fr2501.virage.isabelle.IsabelleFrameworkExtractor;
import com.fr2501.virage.types.ExternalSoftwareUnavailableException;
import com.fr2501.virage.types.FrameworkRepresentation;

public class VirageExtractJob extends VirageJobWithExplicitResult<FrameworkRepresentation> {
  private String sessionName;
  private String path;
  
  public VirageExtractJob(VirageUserInterface issuer, String path, String sessionName) {
    super(issuer);
    
    this.sessionName = sessionName;
    this.path = path;
  }

  @Override
  protected void concreteExecute() throws ExternalSoftwareUnavailableException {
    IsabelleFrameworkExtractor extractor = new IsabelleFrameworkExtractor();
    FrameworkRepresentation framework = extractor.extract(this.path, this.sessionName);
    framework.setTheoryPath(this.path);
    framework.setSessionName(this.sessionName);
    
    this.result = framework;
  }

  @Override
  public boolean externalSoftwareAvailable() {
    return (ConfigReader.getInstance().hasIsabelle());
  }
}
