package com.polus.fibicomp.coi.dto;

import lombok.*;

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
}
