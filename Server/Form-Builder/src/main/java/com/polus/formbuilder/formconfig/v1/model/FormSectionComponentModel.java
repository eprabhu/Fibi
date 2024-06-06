package com.polus.formbuilder.formconfig.v1.model;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FormSectionComponentModel {
	
	private int componentId;
    private int sectionId;
    private Integer formBuilderId;
    private String componentType;
    private Integer componentOrder;
    private String componentData;
    private String componentRefId;
    private String description;
    private String componentHeader;
    private String componentFooter;
    private String isActive;
    private Date updateTimestamp;
    private String updateUser;
    private String isMandatory;
    private String validationMessage;
    private String label;
}
