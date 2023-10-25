package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OPAPersonEntityInfoDTO {
	
	private Integer opaDisclPersonEntityId;
	private Integer personEntityId;
    private String personId;
    private Integer entityNumber;
    private String entityName;
    private String entityType;
    private String entityStatus;
    private String entityRiskCategory;
    private String countryName;
    private String relationship;
    private Character isRelationshipActive;
    private String sfiVersionStatus;    
    private Date involvementStartDate;
    private Date involvementEndDate;
    
}
