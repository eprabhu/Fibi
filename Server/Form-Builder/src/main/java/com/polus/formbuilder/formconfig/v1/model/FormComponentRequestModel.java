package com.polus.formbuilder.formconfig.v1.model;

import lombok.Data;

@Data
public class FormComponentRequestModel {
	
	private int formBuilderSectCompId;
    private int formBuilderSectionId;
    private int formBuilderId;
    private String componentTypeCode;
    private Integer componentOrderNumber;
    private String componentData;
    private String componentRefId;
    private String description;
    private String headerInstruction;
    private String footerInstruction;
    private String isActive;

}
