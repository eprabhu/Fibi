package com.polus.formbuilder.programmedelement;

public interface ProgrammedElementModel{

	default String getActionType() {
		return "SAVE";
	}	
}
