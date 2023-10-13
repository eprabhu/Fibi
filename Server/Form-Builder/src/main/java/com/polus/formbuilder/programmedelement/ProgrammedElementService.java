package com.polus.formbuilder.programmedelement;

import org.springframework.stereotype.Service;

@Service
public class ProgrammedElementService{
	
	public ProgrammedElementModel getBlankResponse(String elementName) {
		ProgrammedElement object = ProgrammedElementFactory.createProgrammedElement(elementName);
		return object.getBlankResponse();
	}
	
	public ProgrammedElementModel getResponse(String elementName,
													  ProgrammedElementModel request ) {
		ProgrammedElement object = ProgrammedElementFactory.createProgrammedElement(elementName);
		return object.getResponse(request);
	}
	
	public ProgrammedElementModel performAction(String elementName,
													  ProgrammedElementModel request ) {
	ProgrammedElement object = ProgrammedElementFactory.createProgrammedElement(elementName);	
	return object.action(request);
	
	}
}
