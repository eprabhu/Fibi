package com.polus.formbuilder.model;

import lombok.Data;

@Data
public class FormComponentFetchRequest{
	
	private Integer formBuilderId;
	
	private String documentOwnerPersonId;
	
	private String moduleItemCode;
	
	private String moduleSubItemCode;
	
	private String moduleItemKey;
	
	private String moduleSubItemKey;
	
	private Integer componentId;
	
	private String componentType;
	
	private String componentRefId;
	
	private String componentData;
	
	
	
}
