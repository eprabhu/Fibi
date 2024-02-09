package com.polus.formbuilder.programmedelement;

import org.springframework.stereotype.Service;

@Service
public interface ProgrammedElement {

	abstract ProgrammedElementModel getBlankResponse();
	
	abstract ProgrammedElementModel getResponse(
												ProgrammedElementModuleDetails moduleDetails,
												ProgrammedElementModel request) throws InterruptedException;
	
	abstract ProgrammedElementModel save(
											ProgrammedElementModuleDetails moduleDetails,
											ProgrammedElementModel request);
	
}
