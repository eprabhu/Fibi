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
public class FormUsageModel {
    private int formUsageId;
    private int formBuilderId;
    private Integer formOrderNumber;
    private String moduleCode;
    private String subModuleCode;
    private Integer businessRuleId;
    private String description;
    private String isActive;
    private Date createTimestamp;
    private String createUser;
    private Date updateTimestamp;
    private String updateUser;
}
