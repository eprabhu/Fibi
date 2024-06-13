package com.polus.formbuilder.formconfig.v1.model;

import lombok.Data;

@Data
public class FormComponentRequestModel {
	
	private int componentId;
    private int sectionId;
    private int formBuilderId;
    private String componentType;
    private Integer componentOrder;
    private String componentData;
    private String componentRefId;
    private String description;
    private String componentHeader;
    private String componentFooter;
    private String isActive;
    private String isMandatory;
    private String validationMessage;
    private String label;

}
