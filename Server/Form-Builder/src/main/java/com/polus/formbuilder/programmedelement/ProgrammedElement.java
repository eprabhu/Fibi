package com.polus.formbuilder.programmedelement;

public interface ProgrammedElement {

	abstract ProgrammedElementModel getBlankResponse();
	
	abstract ProgrammedElementModel getResponse(ProgrammedElementModel request);
	
	abstract ProgrammedElementModel action(ProgrammedElementModel request);
	
}
