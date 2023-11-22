package com.polus.formbuilder.formconfig.v1.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FormSectionRequestModel {

 	private int formBuilderSectionId;
    private int formBuilderId;
    private String sectionName;
    private Integer sectionOrderNumber;
    private Integer businessRuleId;
    private String description;
    private String helpText;
    private String headerInstruction;
    private String footerInstruction;
    private String isActive;
          
}
