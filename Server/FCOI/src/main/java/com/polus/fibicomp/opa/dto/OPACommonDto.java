package com.polus.fibicomp.opa.dto;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class OPACommonDto {

    private Integer opaDisclosureId;
    private String opaDisclosureNumber;
    private String adminGroupName;
    private String adminPersonName;
    private String reassignedAdminPersonName;
    private String comment;
    private String description;
    private String updateUserFullName;
    private String reviewerFullName;
    private String reviewLocationType;
    private String reviewStatusType;
    private String personId;
}
