package com.polus.formbuilder.formconfig.v1.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FormSectionRequestModel {

 	private int sectionId;
    private int formBuilderId;
    private String sectionName;
    private Integer sectionOrder;
    private Integer sectionBusinessRule;
    private String sectionDescription;
    private String sectionHelpText;
    private String sectionHeader;
    private String sectionFooter;
    private String isActive;

}
