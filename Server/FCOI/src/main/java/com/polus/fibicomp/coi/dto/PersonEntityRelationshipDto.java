package com.polus.fibicomp.coi.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PersonEntityRelationshipDto {

    private Integer personEntityId;
    private Integer entityId;
    private String entityName;
    private Integer entityNumber;
    private String countryName;
    private String validPersonEntityRelType;
    private String entityType;
    private String entityRiskCategory;
    private String personEntityVersionStatus;
    private Boolean isFormCompleted;
	private Date involvementStartDate;
	private Date involvementEndDate;

}
