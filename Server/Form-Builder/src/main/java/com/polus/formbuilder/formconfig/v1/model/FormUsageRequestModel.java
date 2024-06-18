package com.polus.formbuilder.formconfig.v1.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FormUsageRequestModel {

    private int formUsageId;
    private int formBuilderId;
    private String formBuilderNumber;
    private Integer formOrderNumber;
    private String moduleCode;
    private String subModuleCode;
    private Integer businessRuleId;
    private String description;
    private String isActive;

}
